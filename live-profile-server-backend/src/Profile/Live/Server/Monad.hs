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

import Control.Monad.Trans
import Control.Monad.Except                 (ExceptT, MonadError)
import Control.Monad.Reader                 
import Data.Monoid                          ((<>))
import Database.Persist.Sql    
import Servant                              
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
-- We wrap the standard Servant monad with 'ReaderT AppState', which gives us
-- access to the application state and configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype App a = App { 
    runApp :: ReaderT AppState (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader AppState,
               MonadError ServantErr, MonadIO)

instance AuthMonad App where 
  getAuthConfig = asks appAuth
  liftAuthAction = App . lift

-- | If the value is 'Nothing', throw 400 response
require :: T.Text -> Maybe a -> App a
require info Nothing = throwError $ err400 { errBody = BS.fromStrict (T.encodeUtf8 info) <> " is required" }
require _ (Just a) = return a 

-- | Getting config from global state
getConfig :: App Config 
getConfig = asks appConfig

-- | Getting config part from global state
getsConfig :: (Config -> a) -> App a 
getsConfig getter = asks (getter . appConfig)

-- | Execute database transaction
runDB :: SqlPersistT IO a -> App a
runDB query = do
  pool <- asks appPool
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