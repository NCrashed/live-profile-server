{-|
Module      : Profile.Live.Server.Application.Connection
Description : Implementation of Connection API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application.Connection(
    connectionServer
  ) where

import Data.Proxy 
import Servant.API 
import Servant.API.REST.Derive.Server
import Servant.API.REST.Derive.Server.Vinyl()
import Servant.Server 

import Profile.Live.Server.API.Connection 
import Profile.Live.Server.Monad 

connectionServer :: ServerT ConnectionAPI App 
connectionServer = restServer (Proxy :: Proxy '[ 'GET, 'POST, 'PUT, 'PATCH, 'DELETE]) 
  (Proxy :: Proxy Connection) (Proxy :: Proxy "connection")
  (Proxy :: Proxy App)