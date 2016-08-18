{-|
Module      : Profile.Live.Server.Application
Description : Creation of WAI application
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application(
  -- * WAI application
    liveProfileApp
  -- * Subservers
  , coreServer
  , documentedServer
  , liveProfileServer
  ) where

import Control.Monad.Except 
import Control.Monad.State.Strict
import Network.Wai (Application)
import Servant
import Servant.Server.Auth.Token 

import Profile.Live.Server.API
import Profile.Live.Server.Application.Connection
import Profile.Live.Server.Application.Session
import Profile.Live.Server.Config 
import Profile.Live.Server.Monad 

-- | Handlers for core API that the server implements
coreServer :: AppState -> Server CoreLiveProfileAPI
coreServer app = enter (convertApp app) server 
  where 
  server = 
         connectionServer
    :<|> sessionServer

-- | Implementation of documented server
documentedServer :: AppState -> Server DocumentedLiveProfileAPI
documentedServer app = authServer (appAuth app) :<|> coreServer app

-- | Implmenetation of full profile API
liveProfileServer :: AppState -> Server LiveProfileAPI 
liveProfileServer app = documentedServer app :<|> files app

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: AppState -> App :~> ExceptT ServantErr IO
convertApp app = Nat (flip evalStateT app . runApp)

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: AppState -> Application
files app = serveDirectory (configStatic . appConfig $ app)

-- | Create WAI application for live profiler
liveProfileApp :: AppState -> Application 
liveProfileApp = serve liveProfileAPI . liveProfileServer