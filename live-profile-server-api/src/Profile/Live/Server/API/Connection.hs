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
  , connectionAPI
  -- * Data types
  , Connection
  , ConnectionPatch(..)
  -- * Helpers
  , connectionOperations
  ) where 

import Control.Lens
import Data.Monoid
import Data.Proxy
import Data.Swagger
import Data.Swagger.Internal.Schema
import Data.Text 
import Data.Time
import Data.Vinyl.Derived
import Servant.API
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Internal.Schema 
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Named
import Servant.API.REST.Derive.TH
import Servant.Swagger 

-- | Connection to remote application
type Connection = FieldRec '[
    '("name", Text)
  , '("host", Text)
  , '("port", Word)
  , '("lastUsed", Maybe UTCTime)
  ]

instance Named Connection where 
  getName _ = "Connection"

-- | Correspoinding patch record
$(declareVinylPatch ''Connection)

-- | API about connections to remote Haskell applications that we profile
type ConnectionAPI = "connection" :> (
      RESTFull Connection "connection"
  :<|> "list" 
    :> PageParam
    :> PageSizeParam
    :> TokenHeader' '["read-connection"]
    :> Get '[JSON] (PagedList (Id Connection) Connection) 
  )

-- Needed due bug with `Can't find interface-file declaration for variable $tc'(,)`
instance {-# OVERLAPPING #-} ToSchema (PagedList (Id Connection) Connection) where 
  declareNamedSchema p = do
    s <- genericDeclareNamedSchema (schemaOptionsDropPrefix "pagedList") p
    return $ rename nm s
    where 
    nm = Just $ "PagedList " <> iname <> " " <> aname
    iname = "Id Connection"
    aname = "Connection"

-- | Helper to carry 'ConnectionAPI' type around
connectionAPI :: Proxy ConnectionAPI
connectionAPI = Proxy 

-- | Select only operations of the Connection API
connectionOperations :: Traversal' Swagger Operation
connectionOperations = operationsOf $ toSwagger (Proxy :: Proxy ConnectionAPI)