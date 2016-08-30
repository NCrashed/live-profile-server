{-|
Module      : Profile.Live.Server.Application.Bined.Model
Description : Implementation of DB related stuff for Bined graph API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Profile.Live.Server.Application.Bined.Model where

import Database.Persist.TH 

share [mkPersist sqlSettings
     , mkDeleteCascade sqlSettings
     , mkMigrate "migrateAll"] [persistLowerCase|

|]
