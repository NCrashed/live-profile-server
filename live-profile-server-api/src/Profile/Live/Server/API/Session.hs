{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Profile.Live.Server.API.Session
Description : Sub API about current stream of events being received from remote app
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.API.Session(
    SessionAPI
  , sessionAPI
  -- * Data types
  , Session
  , SessionPatch(..)
  -- * Helpers
  , sessionOperations
  ) where 

import Control.Lens
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Monoid
import Data.Proxy
import Data.Swagger
import Data.Swagger.Internal.Schema
import Data.Text (Text)
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

import Profile.Live.Server.API.Connection 
import Profile.Live.Server.API.EventLog 

-- | Session to remote application
type Session = FieldRec '[
    '("connection", Id Connection)
  , '("start", UTCTime)
  , '("end", Maybe UTCTime)
  , '("log", EventLogId)
  , '("error", Maybe Text)
  ]
  
instance Named Session where 
  getName _ = "Session"

-- | Correspoinding patch record
$(declareVinylPatch ''Session)

-- | API about sessions to remote Haskell applications that we profile
type SessionAPI = "session" :> (
       RESTFullWith '[ 'GET] Session "session"
  :<|> Capture "session-id" (Id Session)
    :> TokenHeader' '["delete-session"]
    :> Delete '[JSON] Unit
  :<|> "list" 
    :> PageParam
    :> PageSizeParam 
    :> QueryParam "connection" (Id Connection)
    :> TokenHeader' '["read-session"]
    :> Get '[JSON] (PagedList (Id Session) Session) 
  :<|> "connect" 
    :> Capture "connection-id" (Id Connection) 
    :> TokenHeader' '["connect-session"]
    :> Post '[JSON] (OnlyId (Id Session))
  :<|> "disconnect" 
    :> Capture "session-id" (Id Session) 
    :> TokenHeader' '["connect-session"]
    :> Post '[JSON] Unit
  -- Load from file from special folder of server
  :<|> "import" :> "local" 
    :> Capture "connection" (Id Connection)
    :> TokenHeader' '["write-session"]
    :> Post '[JSON] Unit
  )

-- Needed due bug with `Can't find interface-file declaration for variable $tc'(,)`
instance {-# OVERLAPPING #-} ToSchema (PagedList (Id Session) Session) where 
  declareNamedSchema p = do
    s <- genericDeclareNamedSchema (schemaOptionsDropPrefix "pagedList") p
    return $ rename nm s
    where 
    nm = Just $ "PagedList " <> iname <> " " <> aname
    iname = "Id Session"
    aname = "Session"

-- | Proxy to pass around 'SessionAPI'
sessionAPI :: Proxy SessionAPI 
sessionAPI = Proxy 

-- | Select only operations of the Session API
sessionOperations :: Traversal' Swagger Operation
sessionOperations = operationsOf $ toSwagger sessionAPI