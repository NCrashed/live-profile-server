{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Profile.Live.Server.API.Connection
Description : Sub API about connections to haskell apps being profiled
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.API.Connection(
    ConnectionAPI
  -- * Data types
  , Connection
  , ConnectionPatch
  -- * Helpers
  , connectionOperations
  ) where 

import Control.Lens
import Data.Proxy
import Data.Swagger 
import Data.Text 
import Data.Time 
import Data.Vinyl.Derived
import Servant.API
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Named
import Servant.API.REST.Derive.Vinyl()
import Servant.Swagger 

-- | Connection to remote application
type Connection = FieldRec '[
    '("name", Text)
  , '("host", Text)
  , '("port", Word)
  , '("lastUsed", UTCTime)
  ]

instance Named Connection where 
  getName _ = "Connection"

-- | Correspoinding patch record
type ConnectionPatch = FieldRec '[
    '("name", Maybe Text)
  , '("host", Maybe Text)
  , '("port", Maybe Word)
  , '("lastUsed", Maybe UTCTime)
  ]
type instance PatchRec Connection = ConnectionPatch

instance Named ConnectionPatch where 
  getName _ = "ConnectionPatch"

-- | API about connections to remote Haskell applications that we profile
type ConnectionAPI = "connection" :> RESTFull Connection "connection"

-- | Select only operations of the Connection API
connectionOperations :: Traversal' Swagger Operation
connectionOperations = operationsOf $ toSwagger (Proxy :: Proxy ConnectionAPI)