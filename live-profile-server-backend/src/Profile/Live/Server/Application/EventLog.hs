{-|
Module      : Profile.Live.Server.Application.EventLog
Description : Implementation of EventLog API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE BangPatterns #-}
module Profile.Live.Server.Application.EventLog(
    startEventLog
  , addEventLogType
  , addEventLogEvent
  , addEventLogState
  , readEvent 
  , readEventType 
  , readEventLogState
  , readEventLogEvents
  , readEventLogEventTypes
  , readEventLogStates
  , deleteEventLog
  , getEventLogFirstEvent
  , getEventLogLastEvent
  , importEventLog
  , parseEventLog
  , importingList
  , importingCancel
  , ParseStage(..)
  , ParseExit(..)
  , parseEventLogInc
  , importEventLogInc
  , eventLogServer
  ) where 

import Control.Monad 
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Foldable 
import Data.Maybe 
import Data.Monoid
import Data.Text (Text)
import Database.Persist.Sql 
import GHC.RTS.Events as E 
import Servant.API as S
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Server 
import Servant.Server.Auth.Token

import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B 
import qualified GHC.RTS.EventsIncremental as E 

import Profile.Live.Protocol.State
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Events.Model
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils

import Profile.Live.Server.Application.EventLog.Query

-- | Implementation of eventl log sub API
eventLogServer :: ServerT EventLogAPI App 
eventLogServer = listEvents
  :<|> downloadEventLog
  :<|> importingListMethod
  :<|> importingCancelMethod
  :<|> deleteEventLogMethod

-- | Getting events from server with pagination
listEvents :: EventLogId
  -> Maybe Page 
  -> Maybe PageSize
  -> MToken' '["read-eventlog"]
  -> App (PagedList (Id Event) Event)
listEvents ei mp msize token = do 
  guardAuthToken token 
  pagination mp msize $ \page size -> do 
    let filters = [
            EventImplEventLog ==. toKey ei 
          ]
    (es, total) <- runDB $ (,)
      <$> (do
        (is :: [Key EventImpl]) <- selectKeysList filters [Asc EventImplTime, OffsetBy (fromIntegral $ page * size), LimitTo (fromIntegral size)]
        forM is $ (\i -> fmap (WithField (Id $ fromKey i)) <$> readEvent i))
      <*> count filters
    return PagedList {
        pagedListItems = catMaybes es
      , pagedListPages = ceiling $ (fromIntegral total :: Double) / fromIntegral size
      }

-- | Version of 'importingList' with authorisation checks for 
-- server implementation
importingListMethod :: MToken' '["read-eventlog"] -- ^ Authorisation token
  -> App [EventLogImport]
importingListMethod token = do 
  guardAuthToken token 
  runDB importingList 

-- | Version of 'importingCancel' with authorisation checks for 
-- server implementation
importingCancelMethod :: EventLogId 
  ->  MToken' '["write-eventlog"]
  -> App Unit 
importingCancelMethod i token = do 
  guardAuthToken token 
  runDB $ importingCancel i
  return Unit

-- | Server method implementation for `deleteEventLog` with 
-- authorisation
deleteEventLogMethod :: EventLogId 
  -> MToken' '["delete-eventlog"]
  -> App Unit  
deleteEventLogMethod i token = do 
  guardAuthToken token 
  runDB $ deleteEventLog $ toKey i 
  return Unit

-- | Downloading eventlog file from the server
downloadEventLog :: EventLogId -- ^ Id of log
--  -> MToken' '["read-eventlog"] -- ^ Authorisation token
  -> App (Headers '[S.Header "Content-Disposition" Text]
      EventLogFile)
downloadEventLog i {-token-} = do 
--  guardAuthToken token 
  elog <- runDB404 "event log" $ readEventLog (toKey i)
  let payload = B.runPut $ putEventLog elog 
  let filename = showt i <> ".eventlog"
  let hdr = "attachment; filename=\"" <> filename <> "\""
  return $ addHeader hdr $ EventLogFile payload

-- | Insert empty eventlog
startEventLog :: SqlPersistT IO EventLogImplId
startEventLog = insert $ EventLogImpl Nothing Nothing Nothing Nothing

-- | Insert event type into eventlog being recording
addEventLogType :: EventLogImplId -> EventType -> SqlPersistT IO EventTypeImplId
addEventLogType lid t = insert $ toEventTypeImpl lid t

-- | Insert event into eventlog being recording
addEventLogEvent :: EventLogImplId -> Event -> SqlPersistT IO EventImplId 
addEventLogEvent lid e = do
  let (impl, mkInfo) = toEventImpl lid e
  i <- insert impl 
  ei <- insert $ mkInfo i 
  return ei 

-- | Insert state snapshot into eventlog being recording
addEventLogState :: EventLogImplId -> EventlogState -> SqlPersistT IO EventlogStateImplId
addEventLogState lid s = do 
  let (impl, threads, capsets, caps, tasks) = toEventlogStateImpl lid s 
  si <- insert impl 
  mapM_ (void . insert) $ threads si
  forM_ (capsets si) $ \(cs, cps, args, envs) -> do 
    ci <- insert cs 
    mapM_ (void . insert) $ cps ci
    mapM_ (void . insert) $ args ci 
    mapM_ (void . insert) $ envs ci 
  mapM_ (void . insert) $ caps si 
  mapM_ (void . insert) $ tasks si
  return si

-- | Getting event from DB
readEvent :: EventImplId -> SqlPersistT IO (Maybe Event)
readEvent i = do 
  me <- get i
  case me of 
    Nothing -> return Nothing 
    Just e -> do 
      mei <- get $ eventImplSpec e
      return $ fromEventImpl e =<< mei

-- | Read event type from DB
readEventType :: EventTypeImplId -> SqlPersistT IO (Maybe EventType)
readEventType i = do 
  mei <- get i
  return $ fromEventTypeImpl <$> mei

-- | Read eventlog state from DB
readEventLogState :: EventlogStateImplId -> SqlPersistT IO (Maybe EventlogState)
readEventLogState i = do 
  mei <- get i
  ths <- fmap entityVal <$> selectList [ThreadStateImplState ==. i] []
  css' <- selectList [CapsetStateImplState ==. i] []
  css <- forM css' $ \(Entity ci v) -> do 
    cssCaps <- fmap entityVal <$> selectList [CapsetStateCapState ==. ci] []
    cssArgs <- fmap entityVal <$> selectList [CapsetStateArgState ==. ci] []
    cssEnvs <- fmap entityVal <$> selectList [CapsetStateEnvState ==. ci] []
    return (v, cssCaps, cssArgs, cssEnvs)
  cs <- fmap entityVal <$> selectList [CapStateImplState ==. i] []
  ts <- fmap entityVal <$> selectList [TaskStateImplState ==. i] []
  return . join $ fromEventlogStateImpl 
    <$> mei
    <*> pure ths 
    <*> pure css 
    <*> pure cs 
    <*> pure ts 

-- | Getting event types from DB
readEventLogEventTypes :: EventLogImplId -> SqlPersistT IO [EventType]
readEventLogEventTypes i = do 
  es <- fmap entityVal <$> selectList [EventTypeImplEventLog ==. i] [Asc EventTypeImplNum]
  return $ fromEventTypeImpl <$> es

-- | Getting list of states of eventlog from DB
readEventLogStates :: EventLogImplId -> SqlPersistT IO [EventlogState]
readEventLogStates i = do 
  is <- selectKeysList [EventlogStateImplEventLog ==. i] [Asc EventlogStateImplTime]
  catMaybes <$> mapM readEventLogState is

-- | Reading full event log from DB
readEventLog :: EventLogImplId -> SqlPersistT IO (Maybe EventLog)
readEventLog i = do 
  ml <- get i 
  case ml of 
    Nothing -> return Nothing 
    Just _ -> do 
      ets <- readEventLogEventTypes i
      es <- readEventLogEvents i
      return $ Just $ EventLog (E.Header ets) (Data es)

-- | Cascade deletion of all particular eventlog info
deleteEventLog :: EventLogImplId -- ^ Id of log
  -> SqlPersistT IO ()
deleteEventLog = deleteCascade

-- | Import raw event log from memory 
importEventLog :: FilePath -> EventLog -> App EventLogId
importEventLog p (EventLog (E.Header types) (Data es)) = do 
  i <- runDB $ insert $ EventLogImpl (Just 0) (Just p) Nothing Nothing
  let total = fromIntegral $ length types + length es
  runDB $ do
    mapM_ (void . addEventLogType i) types
    let typesPart = fromIntegral (length types) / total 
    update i [EventLogImplImport =. Just typesPart]
  runDB $ do
    mapM_ (void . addEventLogEvent i) es
    let eventsPart = fromIntegral (length es) / total 
    update i [EventLogImplImport =. Just eventsPart]
  runDB $ update i [EventLogImplImport =. Nothing]
  return $ fromKey i 

-- | Parse raw event log and import it incrementaly
importEventLogInc :: FilePath -> B.ByteString -> App (Either ParseExit EventLogId)
importEventLogInc fn bs = do 
  i <- runDB $ insert $ EventLogImpl (Just 0) (Just fn) Nothing Nothing
  res <- parseEventLogInc bs $ consumer i
  case res of 
    Left ParseCancel -> runDB $ delete i 
    Left (ParseError er) -> runDB $ update i [EventLogImplImportFailed =. Just er]
    _ -> runDB $ update i [EventLogImplImport =. Nothing]
  return $ const (fromKey i) <$> res
  where 
  consumer i p r = do
    canceled <- isImportCanceled i
    unless canceled $ do
      updateImportPercent i p
      case r of 
        ParseHeader (E.Header types) -> mapM_ (void . addEventLogType i) types
        ParseEvent e -> void $ addEventLogEvent i e 
    return canceled

-- | Parse eventlog from memory, allow partial logs
parseEventLog :: B.ByteString -> Either String EventLog 
parseEventLog bs = let 
  parser = E.newParserState `E.pushBytes` (B.toStrict bs)
  in case go False [] parser of 
    Left s -> Left s 
    Right (es, parser') -> case E.readHeader parser' of 
      Nothing -> Left "Missing header"
      Just h -> Right $ EventLog h (Data es)
  where 
  go !incomplete !acc !parser = let 
    (res, parser') = E.readEvent parser 
    in case res of 
      E.Item a -> go False (a : acc) parser'
      E.Incomplete -> if incomplete 
        then Right (reverse acc, parser') 
        else go True acc parser'
      E.Complete -> Right (reverse acc, parser')
      E.ParseError err -> Left err

-- | Helper data type for `parseEventLogInc`
data ParseStage = 
    ParseEvent !E.Event 
  | ParseHeader !E.Header

-- | Helper data type for `parseEventLogInc`
data ParseExit = ParseError String | ParseCancel 

-- | Parse eventlog incrementally from memory, allow partial logs
parseEventLogInc :: B.ByteString -- ^ Data of log
  -> (Double -> ParseStage -> SqlPersistT IO Bool) 
    -- ^ Reaction to partial result, first parameter is percent of completion.
    -- If method returns 'False' the parsing is stopped.
  -> App (Either ParseExit ())
parseEventLogInc bs consumer = do
  res <- parseChunkes
  case res of 
    Left er -> return $ Left er 
    Right parser -> case E.readHeader parser of 
      Nothing -> return $ Left $ ParseError "Missing header"
      Just h -> do
        cancel <- runDB $ consumer 1.0 $ ParseHeader h 
        return $ if cancel then Left ParseCancel
          else Right ()
  where 
  totalLength = fromIntegral (B.length bs)

  parseChunkes = do
    (res, compl) <- foldlM parseChunk (Right E.newParserState, 0) $ B.toChunks bs
    fst <$> parseChunk (res, compl) BS.empty

  parseChunk acc@(Left _, _) _ = return acc
  parseChunk (Right !p, !compl) !chunk = do
    p' <- go $ p `E.pushBytes` chunk
    return (p', compl')
    where 
    compl' = compl + fromIntegral (BS.length chunk) / totalLength
    go !parser = do 
      let (res, parser') = E.readEvent parser 
      case res of 
        E.Item e -> do
          cancel <- runDB $ consumer compl' $ ParseEvent e
          if cancel then return $ Left ParseCancel
            else go parser'
        E.Incomplete -> return $ Right parser'
        E.Complete -> return $ Right parser'
        E.ParseError err -> return . Left $ ParseError err



-- | Get list of logs that are currently imports
--
-- Canceled imports aren't returned.
importingList :: SqlPersistT IO [EventLogImport]
importingList = do
  elogs <- selectList [
      EventLogImplImport !=. Nothing
    , EventLogImplImportCancel ==. Nothing] []
  return $ flip fmap elogs $ \(Entity i EventLogImpl{..}) -> EventLogImport {
      eventLogImportId = fromKey i 
    , eventLogImportFileName = fromMaybe "unknown" eventLogImplImportFileName
    , eventLogImportPercent = fromMaybe 0 eventLogImplImport
    , eventLogImportError = eventLogImplImportFailed
    }

-- | Cancel import process for givent eventlog
importingCancel :: EventLogId -> SqlPersistT IO ()
importingCancel i = do 
  mlog <- get (toKey i :: EventLogImplId)
  whenJust mlog $ \_ -> 
    update (toKey i) [EventLogImplImportCancel =. Just True]

-- | Checks wether eventlog import is canceled
isImportCanceled :: EventLogImplId -> SqlPersistT IO Bool
isImportCanceled i = do 
  me <- get i 
  return $ maybe False (fromMaybe False . eventLogImplImportCancel) me

-- | Update state of import of eventlog
updateImportPercent :: EventLogImplId -> Double -> SqlPersistT IO ()
updateImportPercent i p = update i [EventLogImplImport =. Just p]

