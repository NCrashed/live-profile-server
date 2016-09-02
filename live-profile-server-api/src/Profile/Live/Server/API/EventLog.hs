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
  , EventLogFile(..)
  , EventLogImport(..)
  , EventLogAPI
  , eventLogAPI
  , eventLogOperations
  ) where 

import Control.Lens
import Data.Aeson
import Data.Aeson.Unit
import Data.Proxy 
import Data.Swagger
import Data.Text
import GHC.Generics 
import Servant.API as S
import Servant.API.Auth.Token 
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Swagger 

import GHC.RTS.Events

import qualified Data.ByteString.Lazy as BS

import Profile.Live.Server.Utils.DeriveJson
import Profile.Live.Server.Utils.Schema 

-- | Identifier of recorded event log 
type EventLogId = Word 

-- | Wrapper around raw bytes to generate proper swagger scheme for API
newtype EventLogFile = EventLogFile { unEventLogFile :: BS.ByteString }
  deriving (Show, Eq, Generic, MimeRender OctetStream, MimeUnrender OctetStream)

instance ToSchema EventLogFile where 
  declareNamedSchema _ = do 
    return $ NamedSchema Nothing $ mempty 
      & type_ .~ SwaggerString -- TODO: https://github.com/GetShopTV/swagger2/issues/76

-- | Info about imports
data EventLogImport = EventLogImport {
  eventLogImportId :: !EventLogId 
, eventLogImportFileName :: !String
, eventLogImportPercent :: !Double 
, eventLogImportError :: !(Maybe String)
} deriving (Generic)

$(deriveJSON (derivePrefix "eventLogImport") ''EventLogImport)

instance ToSchema EventLogImport where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "eventLogImport"


-- | API for reading event log from server
type EventLogAPI = "eventlog" :> (
       "list"
    :> Capture "eventlog" EventLogId
    :> PageParam
    :> PageSizeParam
    :> TokenHeader' '["read-eventlog"]
    :> Get '[JSON] (PagedList (Id Event) Event)
  -- Download url for eventlog
  :<|> "download"
    :> Capture "eventlog" EventLogId
    -- :> TokenHeader' '["read-eventlog"] -- TODO: cannot attach header to href, find another way to download file
    :> Get '[OctetStream] (
        Headers '[S.Header "Content-Disposition" Text] 
        EventLogFile)
  -- Getting list of in process importing of eventlogs
  :<|> "importing" 
    :> TokenHeader' '["read-eventlog"]
    :> Get '[JSON] [EventLogImport] 
  -- Canceling import
  :<|> "importing" :> "cancel"
    :> Capture "eventlog" EventLogId 
    :> TokenHeader' '["write-eventlog"]
    :> Post '[JSON] Unit 
  -- Delete eventlog
  :<|> Capture "eventlog" EventLogId
    :> TokenHeader' '["delete-eventlog"]
    :> Delete '[JSON] Unit
  )

-- | Value to carry type 'EventLogAPI' around
eventLogAPI :: Proxy EventLogAPI 
eventLogAPI = Proxy 

-- | Select only operations of the Session API
eventLogOperations :: Traversal' Swagger Operation
eventLogOperations = operationsOf $ toSwagger eventLogAPI

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