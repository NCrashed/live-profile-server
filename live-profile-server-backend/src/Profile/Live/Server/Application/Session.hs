{-# OPTIONS_GHC -ddump-if-trace #-}
module Profile.Live.Server.Application.Session(
    sessionServer
  ) where

import Data.Proxy 
import Servant.API 
import Servant.API.REST.Derive.Server
import Servant.API.REST.Derive.Server.Vinyl()
import Servant.Server 

import Profile.Live.Server.API.Session 
import Profile.Live.Server.Monad 

sessionServer :: ServerT SessionAPI App 
sessionServer = restServer (Proxy :: Proxy '[ 'GET ]) 
  (Proxy :: Proxy Session) (Proxy :: Proxy "session")
  (Proxy :: Proxy App)