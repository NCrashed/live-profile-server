{-|
Module      : Profile.Live.Server.Models
Description : Live server generic operations with DB
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Models(
    doMigrations
  ) where

import Database.Persist.Sql

-- | Perform safe migrations of database
doMigrations :: SqlPersistT IO ()
doMigrations = runMigrationUnsafe $ return ()