{-|
Module      : Profile.Live.Server.API
Description : Abstract API of live profile server
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.API(
  -- * API specs
    LiveProfileAPI
  , DocumentedLiveProfileAPI
  , CoreLiveProfileAPI
  , liveProfileAPI
  , documentedLiveProfileAPI
  , coreLiveProfileAPI
  -- * Helpers for docs and generation of code
  , generateSwagger
  ) where

import Control.Lens 
import Data.Aeson
import Data.Proxy 
import Data.Swagger
import Servant.API 
import Servant.API.Auth.Token
import Servant.Swagger 

import qualified Data.ByteString.Lazy as BS 

import Profile.Live.Server.API.Bined
import Profile.Live.Server.API.Connection 
import Profile.Live.Server.API.EventLog 
import Profile.Live.Server.API.Session 

-- | The live profile server consists of documented API and static file serving 
type LiveProfileAPI = DocumentedLiveProfileAPI :<|> Raw
-- | The live profile server API that has corresponding swagger documentation.
type DocumentedLiveProfileAPI = AuthAPI :<|> CoreLiveProfileAPI
-- | API that is provided by the particular server
type CoreLiveProfileAPI = 
       ConnectionAPI 
  :<|> SessionAPI
  :<|> EventLogAPI
  :<|> BinedAPI

-- | Helper to pass around 'LiveProfileAPI'
liveProfileAPI :: Proxy LiveProfileAPI
liveProfileAPI = Proxy

-- | Helper to pass around 'DocumentedLiveProfileAPI'
documentedLiveProfileAPI :: Proxy DocumentedLiveProfileAPI
documentedLiveProfileAPI = Proxy 

-- | Helper to pass around 'CoreLiveProfileAPI'
coreLiveProfileAPI :: Proxy CoreLiveProfileAPI
coreLiveProfileAPI = Proxy 

-- | Generate swagger description of API and store it at specified file
generateSwagger :: FilePath -> IO ()
generateSwagger path = BS.writeFile path . encode $ toSwagger documentedLiveProfileAPI
  & info.title        .~ "API live-profile-server"
  & info.version      .~ "0.1"
  & info.description  ?~ "This is an API for the server for online profiling of Haskell apps"
  & info.license      ?~ "BSD3"
  & host              ?~ "liveprofile.teaspotstudio.ru"
  & applyTagsFor authOperations ["Authorisation" & description ?~ "Authorisation operations"]
  & applyTagsFor connectionOperations ["Connection" & description ?~ "Connection to profiled app operations"]
  & applyTagsFor sessionOperations ["Session" & description ?~ "Session of a connection to profiled app"]
  & applyTagsFor eventLogOperations ["Event log" & description ?~ "Operations with raw events"]
  & applyTagsFor binedOperations ["Bined graph" & description ?~ "Operations with bined graph for event log"]