{-|
Module      : Profile.Live.Server.Config
Description : Configuration of live profile server
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Config(
    Config(..)
  , Environment(..)
  , setLogger
  , makePool
  , loadConfig
  ) where 

import Control.Exception                    (throw)
import Data.Aeson
import Data.FileEmbed                       (embedFile)
import Data.Yaml                            (decodeEither')
import Data.Yaml.Config
import GHC.Generics
import Network.Wai                          (Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Profile.Live.Server.Config.Database
import Profile.Live.Server.Utils.DeriveJson

import qualified Data.ByteString as BS 

-- | Distinghuish thre different environment
data Environment = 
    Development
  | Test
  | Production
  deriving (Generic, Eq, Show, Read)

$(deriveJSON defaultOptions ''Environment)

-- | The Config of server application that contains all required info to start a 
-- SQL connection pool and tweakable behavior.
data Config = Config {
-- | DB connection configuration
  configDatabase :: DatabaseConfig
-- | Server operation mode
, configEnvironment :: Environment
-- | Location of server static files
, configStatic :: FilePath 
} deriving (Generic, Show)

$(deriveJSON (derivePrefix "config") ''Config)

-- | Logging behavior
setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

-- | Load compile time config
compileTimeConfigBS :: BS.ByteString
compileTimeConfigBS = $(embedFile "config/config.yaml")

-- | Parse compile time config
compileTimeConfig :: Value
compileTimeConfig = either throw id $ decodeEither' compileTimeConfigBS

-- | Load config or use compile time version
loadConfig :: Maybe FilePath -> IO Config 
loadConfig mp = loadYamlSettings paths [compileTimeConfig] useEnv
  where 
  paths = maybe [] pure mp 