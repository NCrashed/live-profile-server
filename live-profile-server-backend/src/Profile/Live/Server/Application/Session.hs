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

import Control.Monad 
import Control.Monad.IO.Class 
import Data.Proxy 
import Data.Time 
import Database.Persist
import Servant.API 
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Server
import Servant.API.REST.Derive.Server.Vinyl
import Servant.Server 

import qualified Data.HashMap.Strict as H 

import Profile.Live.Server.API.Session 
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils

sessionServer :: ServerT SessionAPI App 
sessionServer = restServer (Proxy :: Proxy '[ 'GET ]) 
  (Proxy :: Proxy Session) (Proxy :: Proxy "session")
  (Proxy :: Proxy App)

-- | Get currently running sessions from applicaiton state
getRunningSessions :: App [Id Session]
getRunningSessions = do 
  m <- getSessions
  return $ fmap Id $ H.keys m

-- | If server terminates, all sessions are closed, but they
-- could be not closed in DB. The function checks DB and 
-- closes such hanged sessions.
sanitizeSessions :: App ()
sanitizeSessions = do
  m <- getSessions
  runDB $ do 
    let endField = DBField (Proxy :: Proxy '("end", Maybe UTCTime)) :: EntityField Session (Maybe UTCTime)
    ess <- selectList [endField ==. Nothing] []
    t <- liftIO getCurrentTime
    forM_ ess $ \(Entity i es) -> whenNothing (H.lookup (unId $ unVKey i) m) $ do
      update i [endField =. Just t]