{-|
Module      : Profile.Live.Server.Config.Database
Description : DB configuration and connection pool creation
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Config.Database(
    DatabaseConfig(..)
  , makePool
  ) where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics 

import qualified Data.ByteString.Char8 as BS 
import qualified Database.Persist.Postgresql as PG

import Profile.Live.Server.Utils.DeriveJson

-- | Database related configuration, used to create connection pool
data DatabaseConfig = DatabaseConfig {
  databaseDatabase :: Text -- ^ Name of a DB
, databaseHost :: Text -- ^ Host where a DB listens
, databasePort :: Int -- ^ Port where a DB listens
, databaseUser :: Text -- ^ DB user's login
, databasePassword :: Text -- ^ DB user's password
, databasePoolSize :: Int -- ^ Number of workers in pool
} deriving (Generic, Show)

$(deriveJSON (derivePrefix "database") ''DatabaseConfig)

-- | This function creates a 'ConnectionPool' from the configuration
makePool :: DatabaseConfig -> IO PG.ConnectionPool
makePool DatabaseConfig{..} =
  runStdoutLoggingT $ PG.createPostgresqlPool prodStr databasePoolSize
  where 
  prodStr = BS.unwords [
      "host=" <> encodeUtf8 databaseHost
    , "port=" <> BS.pack (show databasePort)
    , "user=" <> encodeUtf8 databaseUser
    , "password=" <> encodeUtf8 databasePassword
    , "dbname=" <> encodeUtf8 databaseDatabase
    ]
