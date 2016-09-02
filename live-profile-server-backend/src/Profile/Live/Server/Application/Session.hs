{-|
Module      : Profile.Live.Server.Application.Session
Description : Implementation of Session API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application.Session(
    sessionServer
  -- * Helpers
  , getRunningSessions
  , sanitizeSessions
  , importFakeSession
  , importLocal
  ) where

import Control.Lens 
import Control.Monad 
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Maybe 
import Data.Monoid
import Data.Proxy 
import Data.Text (Text)
import Data.Text.Encoding 
import Data.Time 
import Data.Vinyl
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import GHC.RTS.Events
import Servant.API 
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Server
import Servant.API.REST.Derive.Server.Vinyl
import Servant.Server
import Servant.Server.Auth.Token 
import System.Directory
import System.Directory.Tree
import System.FilePath
import System.Socket 
import System.Socket.Family.Inet6
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream

import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H

import Profile.Live.Client
import Profile.Live.Server.API.Connection
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.API.Session 
import Profile.Live.Server.Application.EventLog
import Profile.Live.Server.Config
import Profile.Live.Server.Error
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils
import Profile.Live.Termination

sessionServer :: ServerT SessionAPI App 
sessionServer = restServer (Proxy :: Proxy '[ 'GET ]) 
  (Proxy :: Proxy Session) (Proxy :: Proxy "session")
  (Proxy :: Proxy App)
  :<|> deleteSessionMethod
  :<|> listSessions
  :<|> connectMethod
  :<|> disconnectMethod
  :<|> importLocalMethod

-- | Deleteion of session from server
deleteSessionMethod :: Id Session 
  -> MToken' '["delete-session"]
  -> App Unit 
deleteSessionMethod i token = do 
  guardAuthToken token 
  closeSession i 
  runDB $ deleteSession i 
  return Unit

-- | Open a session with given connection info
connectMethod :: Id Connection 
  -> MToken' '["connect-session"]
  -> App (OnlyId (Id Session))
connectMethod i token = do 
  guardAuthToken token 
  conn <- runDB404 "connection" $ get (VKey i)
  OnlyField <$> openSession (Entity (VKey i) conn)

-- | Close connection to given session
disconnectMethod :: Id Session 
  -> MToken' '["connect-session"]
  -> App Unit
disconnectMethod i token = do 
  guardAuthToken token 
  closeSession i 
  return Unit

-- | Get currently running sessions from applicaiton state
getRunningSessions :: App [Id Session]
getRunningSessions = fmap H.keys getSessions

-- | If server terminates, all sessions are closed, but they
-- could be not closed in DB. The function checks DB and 
-- closes such hanged sessions.
sanitizeSessions :: App ()
sanitizeSessions = do
  m <- getSessions
  runDB $ do 
    ess <- selectUnclosedSessions
    t <- liftIO getCurrentTime
    forM_ ess $ \(Entity i _) -> whenNothing (H.lookup (unVKey i) m) $ do
      setSessionEndTime (unVKey i) t

-- | Selecting unclosed sessions (with not set end time)
selectUnclosedSessions :: SqlPersistT IO [Entity Session]
selectUnclosedSessions = do 
  let endField = DBField (Proxy :: Proxy '("end", Maybe UTCTime)) :: EntityField Session (Maybe UTCTime)
  selectList [endField ==. Nothing] []

-- | Update session end time in DB
setSessionEndTime :: Id Session -> UTCTime -> SqlPersistT IO ()
setSessionEndTime i t = do 
  ms <- get (VKey i)
  case ms of 
    Nothing -> return ()
    Just sess -> do 
      let endField = DBField (Proxy :: Proxy '("end", Maybe UTCTime)) :: EntityField Session (Maybe UTCTime)
      let mt = sess ^. rlens (Proxy :: Proxy '("end", Maybe UTCTime)) . rfield
      whenNothing mt $ 
        update (VKey i) [endField =. Just t]

-- | Update session end time and stop reason in DB
setSessionEndTimeWithError :: Id Session -> UTCTime -> Text -> SqlPersistT IO ()
setSessionEndTimeWithError i t er = do 
  ms <- get (VKey i)
  case ms of 
    Nothing -> return ()
    Just sess -> do 
      let endField = DBField (Proxy :: Proxy '("end", Maybe UTCTime)) :: EntityField Session (Maybe UTCTime)
      let errField = DBField (Proxy :: Proxy '("error", Maybe Text)) :: EntityField Session (Maybe Text)
      let merr = sess ^. rlens (Proxy :: Proxy '("error", Maybe Text)) . rfield
      whenNothing merr $ 
        update (VKey i) [endField =. Just t, errField =. Just er]

-- | Open connection to remote profile monitor and update internal container
-- of opened sessions. Also inserts new session to RDBMS.
--
-- Throws: 'Error'FailedResolveAddress' if cannot resolve remote host.
openSession :: Entity Connection -> App (Id Session)
openSession (Entity i conn) = do 
  logger <- getLogger
  
  -- Resolve address of monitor
  let host = conn ^. rlens (Proxy :: Proxy '("host", Text)) . rfield
  let port = conn ^. rlens (Proxy :: Proxy '("port", Word)) . rfield
  addr <- resolveAddr host port
  let opts = defaultLiveProfileClientOpts {
          clientTargetAddr = addr
        }

  -- Create mutexes for termination of client thread
  term <- newTerminationPair

  -- Allocate new eventlog 
  el <- runDB startEventLog

  -- Construct session and update state
  t <- liftIO getCurrentTime
  let sess :: Session 
      sess = Field (unVKey i) 
          :& Field t 
          :& Field Nothing 
          :& Field (fromKey el) 
          :& Field Nothing
          :& RNil
  VKey sessId <- runDB $ insert sess 

  -- Define behavior with callbacks
  run <- getAppRunner
  let bhv = ClientBehavior {
            clientOnHeader = run . runDB . 
              mapM_ (void . addEventLogType el) . eventTypes
          , clientOnEvent = run . runDB . void . addEventLogEvent el
          , clientOnService = print -- TODO: add reaction to this
          , clientOnState = run . runDB . void . addEventLogState el
          , clientOnExit = \me -> run $ case me of 
              Nothing -> closeSession sessId 
              Just e -> closeSessionWithError sessId (showt e)
        }

  -- Start client
  tid <- liftIO $ startLiveClient logger opts term bhv
  modifySessions $ H.insert sessId (tid, term)

  return sessId 
  where 
  resolveAddr :: Text -> Word -> App (SocketAddress Inet6)
  resolveAddr host port = do
    rs <- liftIO (getAddressInfo 
      (Just $ encodeUtf8 host) 
      (Just $ encodeUtf8 $ showt port)
      aiV4Mapped 
      :: IO [AddressInfo Inet6 Stream TCP])
    case rs of 
      [] -> throw400' Error'FailedResolveAddress $ "Cannot resolve " <> host <> ":" <> showt port 
      (AddressInfo{..} : _) -> return socketAddress

-- | Close running session of profiling and update app state.
--
-- Warning: Updates session in RDMBS.
closeSession :: Id Session -> App ()
closeSession i = do 
  m <- getSessions
  case H.lookup i m of 
    Nothing -> return ()
    Just (_, term) -> do 
      terminate $ fst term -- Don't wait termination
      modifySessions $ H.delete i
  t <- liftIO getCurrentTime
  runDB $ setSessionEndTime i t

-- | Close running session of profiling and update app state.
--
-- Note: saves provided string as reason of termination.
-- Warning: Updates session in RDMBS.
closeSessionWithError :: Id Session -> Text -> App ()
closeSessionWithError i er = do 
  m <- getSessions
  case H.lookup i m of 
    Nothing -> return ()
    Just (_, term) -> do 
      terminate $ fst term -- Don't wait termination
      modifySessions $ H.delete i
  t <- liftIO getCurrentTime
  runDB $ setSessionEndTimeWithError i t er

-- | Enlisint existing sessions
listSessions :: Maybe Page 
  -> Maybe PageSize
  -> Maybe (Id Connection)
  -> MToken' '["read-session"]
  -> App (PagedList (Id Session) Session)
listSessions mp msize mcon token = do 
  guardAuthToken token 
  pagination mp msize $ \page size -> do 
    let connField = DBField (Proxy :: Proxy '("connection", Id Connection)) :: EntityField Session (Id Connection)
    let filters = catMaybes [
            (connField ==.) <$> mcon
          ]
    (es, total) <- runDB $ (,)
      <$> (do
        (is :: [Key Session]) <- selectKeysList filters [OffsetBy (fromIntegral $ page * size), LimitTo (fromIntegral size)]
        forM is $ (\i -> fmap (WithField i) <$> readResource i) . unVKey)
      <*> count filters
    return PagedList {
        pagedListItems = catMaybes es
      , pagedListPages = ceiling $ (fromIntegral total :: Double) / fromIntegral size
      }

-- | Stoping and deleting info about a session
deleteSession :: Id Session -> SqlPersistT IO ()
deleteSession i = do 
  msess <- get (VKey i)
  case msess of 
    Nothing -> return ()
    Just sess -> do 
      let logi = sess ^. rlens (Proxy :: Proxy '("log", EventLogId)) . rfield

      deleteEventLog $ toKey logi
      deleteResource i 

-- | Import all eventlog files from server special folder
importLocalMethod :: Id Connection 
  -> MToken' '["write-session"]
  -> App Unit
importLocalMethod i token = do 
  guardAuthToken token 
  _ <- appFork "Import: " $ importLocal i
  return Unit

-- | Importing a eventlog for connection with creation of fake session
importFakeSession :: Id Connection -> FilePath -> B.ByteString -> App (Either ParseExit (Id Session))
importFakeSession cid fn elog = do 
  -- Import eventlog
  meid <- importEventLogInc fn elog
  case meid of 
    Left er -> return $ Left er
    Right eid -> do 
      -- Construct fake session 
      t <- liftIO getCurrentTime
      let sess :: Session 
          sess = Field cid
              :& Field t 
              :& Field (Just t) 
              :& Field eid
              :& Field Nothing
              :& RNil
      VKey sessId <- runDB $ insert sess 
      return $ Right sessId 

-- | Import all eventlog files from server special folder
importLocal :: Id Connection -> App ()
importLocal i = do 
  ImportConfig{..} <- getsConfig configImport
  liftIO $ do 
    createDirectoryIfMissing True importConfigFolder
    createDirectoryIfMissing True importConfigSuccess
    createDirectoryIfMissing True importConfigFailure
  importLog $ "Start local import from " <> showl importConfigFolder
  (_ :/ dTree) <- liftIO $ readDirectoryWithL B.readFile importConfigFolder
  case dTree of 
    Failed{..} -> importLog $ "Failed to open folder, reason: " <> showl err 
    File{..} -> importLog "Failed, not a directory"
    Dir{..} -> forM_ (flatFiles contents) $ \(nm, bs) ->
      processFile nm bs importConfigFolder importConfigSuccess
      `catchAll`   
      (\e -> do
        importLog $ "Failed: " <> showl e 
        moveToFailure nm importConfigFolder importConfigFailure )
  where 
  importLog = appLog . (\s -> "Import: " <> s <> "\n") 

  processFile name bs f fSucc = do 
    importLog $ "Importing file " <> showl name
    res <- importFakeSession i name bs
    case res of 
      Left (ParseError er) -> fail er 
      _ -> return ()
    moveToSuccess name f fSucc
    importLog $ "Finished importing file " <> showl name

  moveToSuccess name f fSucc = liftIO $ renameFile (f </> name) (fSucc </> name)
  moveToFailure name f fFail = liftIO $ renameFile (f </> name) (fFail </> name)

  flatFiles nodes = catMaybes $ extractFile <$> nodes
    where 
    extractFile node = case node of 
      File{..} -> Just (name, file)
      _ -> Nothing
