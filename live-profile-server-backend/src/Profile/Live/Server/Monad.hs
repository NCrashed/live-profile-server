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
  -- * Helpers
  -- ** Config helpers
  , getConfig
  , getsConfig
  -- ** DB helpers
  , runDB 
  , runDB404
  , guardExist
  -- ** Generic helpers
  , require
  ) where

import Control.Monad.Except                 (ExceptT, MonadError, runExceptT)
import Control.Monad.State.Strict as S
import Data.Monoid                          ((<>))
import Database.Persist.Sql    
import Servant                              
import Servant.API.REST.Derive.Server
import Servant.Server.Auth.Token.Config

import qualified Data.ByteString.Lazy as BS 
import qualified Data.Text as T 
import qualified Data.Text.Encoding as T 

import Profile.Live.Server.Config
import Profile.Live.Server.Config.Auth 

import Servant.Server.Auth.Token 

-- | Global state of application
data AppState = AppState {
  -- | Connection pool for the DB 
  appPool :: ConnectionPool 
  -- | Authorisation configuration
, appAuth :: AuthConfig
  -- | Used configuration of the server
, appConfig :: Config 
}

-- | Make initial application state
initAppState :: Config -> IO AppState 
initAppState cfg@Config{..} = do 
  pool <- makePool configDatabase
  let acfg = makeAuthConfig pool configAuth 
  return AppState {
      appPool = pool 
    , appAuth = acfg 
    , appConfig = cfg
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
               MonadError ServantErr, MonadIO)

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
  state <- S.get
  return $ printException . flip evalStateT state . runApp
  where 
    printException ma = do 
      r <- runExceptT ma
      case r of 
        Left er -> fail $ show er 
        Right a -> return a