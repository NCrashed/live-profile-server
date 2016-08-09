module Profile.Live.Server.Application.Connection(
    connectionServer
  ) where

import Servant.API.REST.Derive.Server
import Servant.Server 

import Profile.Live.Server.API.Connection 
import Profile.Live.Server.Monad 

connectionServer :: ServerT ConnectionAPI App 
connectionServer = restServer