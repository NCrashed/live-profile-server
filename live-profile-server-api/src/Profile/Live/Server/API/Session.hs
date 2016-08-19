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
  -- * Data types
  , Session
  , SessionPatch(..)
  -- * Helpers
  , sessionOperations
  ) where 

import Control.Lens
--import Data.Aeson.Unit
import Data.Proxy
import Data.Swagger 
--import Data.Text 
import Data.Time 
import Data.Vinyl.Derived
import Servant.API
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Named
import Servant.API.REST.Derive.TH
import Servant.Swagger 

import Profile.Live.Server.API.Connection 

-- | Session to remote application
type Session = FieldRec '[
    '("connection", Id Connection)
  , '("start", UTCTime)
  , '("end", Maybe UTCTime)
  ]

instance Named Session where 
  getName _ = "Session"

-- | Correspoinding patch record
$(declareVinylPatch ''Session)

-- | API about sessions to remote Haskell applications that we profile
type SessionAPI = "session" :> (
       RESTFullWith '[ 'GET] Session "session"
  -- :<|> "connect" :> Capture "connection-id" (Id Connection) :> Post '[JSON] (Id Session)
  -- :<|> "disconnect" :> Capture "session-id" (Id Session) :> Post '[JSON] Unit
  )

-- | Select only operations of the Session API
sessionOperations :: Traversal' Swagger Operation
sessionOperations = operationsOf $ toSwagger (Proxy :: Proxy SessionAPI)