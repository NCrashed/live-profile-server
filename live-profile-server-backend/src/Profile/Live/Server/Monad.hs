{-|
Module      : Profile.Live.Server.Monad
Description : Definition of server's monad handler
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Monad(
  -- * Application state
    AppState(..)
  , initAppState
  -- * Application monad
  , App(..)
  , getAppRunner
  , runAppInIO
  , appFork
  -- * Helpers
  -- ** Config helpers
  , getConfig
  , getsConfig
  -- ** Sessions helpers
  , getSessions
  , modifySessions
  -- ** Logger helpers
  , getLogger
  , appLog
  , showl
  -- ** DB helpers
  , runDB 
  , runDB404
  , guardExist
  -- ** Generic helpers
  , require
  , pagination
  ) where

import Control.Concurrent                   (ThreadId, forkIO)
import Control.Monad.Catch
import Control.Monad.Except                 (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.State.Strict as S
import Data.Maybe
import Data.Monoid                          ((<>))
import Database.Persist.Sql    
import Servant                              
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Server
import Servant.Server.Auth.Token 
import Servant.Server.Auth.Token.Config
import System.Log.FastLogger

import qualified Data.ByteString.Lazy as BS 
import qualified Data.HashMap.Strict as H 
import qualified Data.Text as T 
import qualified Data.Text.Encoding as T 

import Profile.Live.Protocol.Utils
import Profile.Live.Server.API.Session 
import Profile.Live.Server.Config
import Profile.Live.Server.Config.Auth 
import Profile.Live.Termination

-- | Container for current opened sessions
type SessionsMap = H.HashMap (Id Session) (ThreadId, TerminationPair)

-- | Global state of application
data AppState = AppState {
  -- | Connection pool for the DB 
  appPool :: !ConnectionPool 
  -- | Authorisation configuration
, appAuth :: !AuthConfig
  -- | Used configuration of the server
, appConfig :: !Config 
  -- | Mapping of currently running sessions of profiling
, appSessions :: !SessionsMap
  -- | Application logger
, appLogger :: !LoggerSet
}

-- | Make initial application state
initAppState :: Config -> IO AppState 
initAppState cfg@Config{..} = do 
  pool <- makePool configDatabase
  logger <- newStdoutLoggerSet defaultBufSize
  let acfg = makeAuthConfig pool configAuth 
  return AppState {
      appPool = pool 
    , appAuth = acfg 
    , appConfig = cfg
    , appSessions = mempty
    , appLogger = logger
    }

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'StateT AppState', which gives us
-- access to the application state and configuration using the 'MonadState'
-- interface's 'get' and 'put' functions.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype App a = App { 
    runApp :: StateT AppState (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadState AppState,
               MonadError ServantErr, MonadIO, MonadThrow, MonadCatch)

instance AuthMonad App where 
  getAuthConfig = gets appAuth
  liftAuthAction = App . lift

instance HasRESTDB App where 
  runRESTDB = runDB

-- | If the value is 'Nothing', throw 400 response
require :: T.Text -> Maybe a -> App a
require info Nothing = throwError $ err400 { errBody = BS.fromStrict (T.encodeUtf8 info) <> " is required" }
require _ (Just a) = return a 

-- | Getting config from global state
getConfig :: App Config 
getConfig = gets appConfig

-- | Getting config part from global state
getsConfig :: (Config -> a) -> App a 
getsConfig getter = gets (getter . appConfig)

-- | Execute database transaction
runDB :: SqlPersistT IO a -> App a
runDB query = do
  pool <- gets appPool
  liftIO $ runSqlPool query pool

-- | Run RDBMS operation and throw 404 (not found) error if 
-- the second arg returns 'Nothing'
runDB404 :: T.Text -> SqlPersistT IO (Maybe a) -> App a 
runDB404 info ma = do 
  a <- runDB ma
  case a of 
    Nothing -> throwError $ err404 { errBody = "Cannot find " <> BS.fromStrict (T.encodeUtf8 info) }
    Just a' -> return a' 

-- | Throw 404 if cannot find a element in db
guardExist :: T.Text -> SqlPersistT IO (Maybe a) -> App ()
guardExist info m = do 
  ma <- runDB m 
  case ma of 
    Nothing -> throwError $ err404 { errBody = "Cannot find " <> BS.fromStrict (T.encodeUtf8 info) }
    Just _ -> return ()

-- | Make a function that can run arbitrary 'App' action in 'IO'
--
-- Intended to be used in 'forkIO'.
getAppRunner :: App (App a -> IO a)
getAppRunner = do 
  st <- S.get
  return $ runAppInIO st
  
-- | Helper to run application actions in IO
runAppInIO :: AppState -> App a -> IO a 
runAppInIO st = printException . flip evalStateT st . runApp
  where 
  printException ma = do 
    r <- runExceptT ma
    case r of 
      Left er -> fail $ show er 
      Right a -> return a

-- | Fork a thread in 'App' monad
appFork :: String -> App a -> App ThreadId 
appFork label m = do 
  run <- getAppRunner
  liftIO . forkIO . printExceptions label . run . void $ m

-- | Retrieve mapping from session to threads from app state
getSessions :: App SessionsMap
getSessions = gets appSessions

-- | Update state of sessions 
modifySessions :: (SessionsMap -> SessionsMap) -> App ()
modifySessions f = modify' (\ st -> st { appSessions = f $ appSessions st })

-- | Retrieve application logger from current state
getLogger :: App LoggerSet
getLogger = gets appLogger

-- | Log a message to logger
appLog :: LogStr -> App ()
appLog msg = do
  logger <- gets appLogger 
  liftIO $ pushLogStr logger msg 

-- | Helper to transform type into log message
showl :: Show a => a -> LogStr 
showl = toLogStr . show 

-- | Helper that implements pagination logic
pagination :: Maybe Page -- ^ Parameter of page
  -> Maybe PageSize -- ^ Parameter of page size
  -> (Page -> PageSize -> App a) -- ^ Handler
  -> App a
pagination pageParam pageSizeParam f = do 
  ps <- getsConfig configPageSize
  let page = fromMaybe 0 pageParam 
      pageSize = fromMaybe ps pageSizeParam
  f page pageSize 
