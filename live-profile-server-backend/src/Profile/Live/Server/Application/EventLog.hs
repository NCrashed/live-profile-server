{-|
Module      : Profile.Live.Server.Application.EventLog
Description : Implementation of EventLog API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
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
  , eventLogServer
  ) where 

import Control.Monad 
import Data.Aeson.WithField
import Data.Maybe 
import Data.Monoid
import Database.Persist.Sql 
import GHC.RTS.Events as E 
import Servant.API as S
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Server 
import Servant.Server.Auth.Token
import Data.Text (Text)

import qualified Data.Binary.Put as B

import Profile.Live.Protocol.State
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Events.Model
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils

-- | Implementation of eventl log sub API
eventLogServer :: ServerT EventLogAPI App 
eventLogServer = listEvents
  :<|> downloadEventLog

-- | Insert empty eventlog
startEventLog :: SqlPersistT IO EventLogImplId
startEventLog = insert EventLogImpl

-- | Insert event type into eventlog being recording
addEventLogType :: EventLogImplId -> EventType -> SqlPersistT IO EventTypeImplId
addEventLogType lid t = insert $ toEventTypeImpl lid t

-- | Insert event into eventlog being recording
addEventLogEvent :: EventLogImplId -> Event -> SqlPersistT IO EventImplId 
addEventLogEvent lid e = insert $ toEventImpl lid e

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
  mei <- get i
  return . join $ fromEventImpl <$> mei

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

-- | Getting events from DB
readEventLogEvents :: EventLogImplId -> SqlPersistT IO [Event]
readEventLogEvents i = do 
  es <- fmap entityVal <$> selectList [EventImplEventLog ==. i] [Asc EventImplTime]
  return $ catMaybes $ fromEventImpl <$> es

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
        (is :: [Key EventImpl]) <- selectKeysList filters [OffsetBy (fromIntegral $ page * size), LimitTo (fromIntegral size)]
        forM is $ (\i -> fmap (WithField (Id $ fromKey i)) <$> readEvent i))
      <*> count filters
    return PagedList {
        pagedListItems = catMaybes es
      , pagedListPages = ceiling $ (fromIntegral total :: Double) / fromIntegral size
      }

-- | Downloading eventlog file from the server
downloadEventLog :: EventLogId -- ^ Id of log
  -> MToken' '["read-eventlog"] -- ^ Authorisation token
  -> App (Headers '[S.Header "Content-Disposition" Text]
      EventLogFile)
downloadEventLog i token = do 
  guardAuthToken token 
  elog <- runDB404 "event log" $ readEventLog (toKey i)
  let payload = B.runPut $ putEventLog elog 
  let filename = showt i <> ".eventlog"
  let hdr = "attachment; filename=\"" <> filename <> "\""
  return $ addHeader hdr $ EventLogFile payload


