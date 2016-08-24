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
  ) where

import Control.Lens 
import Control.Monad 
import Control.Monad.IO.Class 
import Data.Aeson.Unit
import Data.Monoid
import Data.Proxy 
import Data.Text (Text)
import Data.Text.Encoding 
import Data.Time 
import Data.Vinyl
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Servant.API 
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Server
import Servant.API.REST.Derive.Server.Vinyl
import Servant.Server 
import System.Socket 
import System.Socket.Family.Inet6
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream

import qualified Data.HashMap.Strict as H 

import Profile.Live.Client
import Profile.Live.Server.API.Connection
import Profile.Live.Server.API.Session 
import Profile.Live.Server.Error
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils
import Profile.Live.Termination

sessionServer :: ServerT SessionAPI App 
sessionServer = restServer (Proxy :: Proxy '[ 'GET ]) 
  (Proxy :: Proxy Session) (Proxy :: Proxy "session")
  (Proxy :: Proxy App)
  :<|> connectMethod
  :<|> disconnectMethod

-- | Open a session with given connection info
connectMethod :: Id Connection 
  -> App (Id Session)
connectMethod i = do 
  conn <- runDB404 "connection" $ get (VKey i)
  openSession (Entity (VKey i) conn)

-- | Close connection to given session
disconnectMethod :: Id Session 
  -> App Unit
disconnectMethod i = do 
  closeSession i 
  t <- liftIO getCurrentTime
  runDB $ setSessionEndTime i t
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
  let endField = DBField (Proxy :: Proxy '("end", Maybe UTCTime)) :: EntityField Session (Maybe UTCTime)
  update (VKey i) [endField =. Just t]

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

  -- Define behavior with callbacks
  let bhv = ClientBehavior {
            clientOnHeader = print
          , clientOnEvent = print
          , clientOnService = print
          , clientOnState = print
        }

  -- Start client
  tid <- liftIO $ startLiveClient logger opts term bhv

  -- Construct session and update state
  t <- liftIO getCurrentTime
  let sess :: Session 
      sess = Field (unVKey i) :& Field t :& Field Nothing :& RNil  
  VKey sessId <- runDB $ insert sess 
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
-- Warning: Doesn't updates session in RDMBS.
closeSession :: Id Session -> App ()
closeSession i = do 
  m <- getSessions
  case H.lookup i m of 
    Nothing -> return ()
    Just (_, term) -> do 
      terminateAndWait term 
      modifySessions $ H.delete i