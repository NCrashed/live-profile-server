{-|
Module      : Profile.Live.Server.API.EventLog
Description : Sub API about recorded eventlog
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Profile.Live.Server.API.EventLog(
    EventLogId
  , EventLogAPI
  , eventLogAPI
  , eventLogOperations
  ) where 

import Control.Lens
import Data.Aeson
import Data.Proxy 
import Data.Swagger
import GHC.Generics 
import Servant.API
import Servant.API.Auth.Token 
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Swagger 

import GHC.RTS.Events

-- | Identifier of recorded event log 
type EventLogId = Word 

-- | API for reading event log from server
type EventLogAPI = "eventlog" :> (
       "list"
    :> Capture "eventlog" EventLogId
    :> PageParam
    :> PageSizeParam
    :> TokenHeader' '["read-eventlog"]
    :> Get '[JSON] (PagedList (Id Event) Event)
  )

-- | Value to carry type 'EventLogAPI' around
eventLogAPI :: Proxy EventLogAPI 
eventLogAPI = Proxy 

-- | Select only operations of the Session API
eventLogOperations :: Traversal' Swagger Operation
eventLogOperations = operationsOf $ toSwagger (Proxy :: Proxy EventLogAPI)

deriving instance Generic CapsetType 
instance ToJSON CapsetType 
instance FromJSON CapsetType 
instance ToSchema CapsetType

deriving instance Generic KernelThreadId 
instance ToJSON KernelThreadId 
instance FromJSON KernelThreadId 
instance ToSchema KernelThreadId

deriving instance Generic MessageTag 
instance ToJSON MessageTag 
instance FromJSON MessageTag 
instance ToSchema MessageTag

deriving instance Generic ThreadStopStatus 
instance ToJSON ThreadStopStatus 
instance FromJSON ThreadStopStatus 
instance ToSchema ThreadStopStatus

deriving instance Generic EventInfo 
instance ToJSON EventInfo 
instance FromJSON EventInfo 
instance ToSchema EventInfo

deriving instance Generic Event 
instance ToJSON Event 
instance FromJSON Event 
instance ToSchema Event