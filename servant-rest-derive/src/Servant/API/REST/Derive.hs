{-|
Module      : Servant.API.REST.Derive
Description : Deriving RESTful API from generic types
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.REST.Derive(
    Id(..)
  , RESTAction(..)
  , RESTPermission
  , PatchRec
  , RESTFull
  , RESTFullWith
  , RESTEndpoint
  ) where 

import Data.Aeson
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Proxy 
import Data.Swagger
import GHC.Generics
import GHC.TypeLits 
import Servant.API 
import Servant.API.Auth.Token 
import Text.Read 
import Web.PathPieces

-- | Unique id of resource
newtype Id a = Id { unId :: Word } 
  deriving (Generic)

instance Show (Id a) where 
  show (Id a) = show a 

instance Read (Id a) where 
  readPrec = Id <$> readPrec 

instance Eq (Id a) where 
  (Id a) == (Id b) = a == b 

instance Ord (Id a) where 
  (Id a) `compare` (Id b) = a `compare` b 

instance ToSchema (Id a) where 
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Word)

instance ToParamSchema (Id a) where 
  toParamSchema _ = toParamSchema (Proxy :: Proxy Word)

instance ToJSON (Id a) where
  toJSON (Id i) = toJSON i 

instance FromJSON (Id a) where
  parseJSON = fmap Id . parseJSON

instance ToHttpApiData (Id a) where 
  toUrlPiece (Id i) = toUrlPiece i 

instance FromHttpApiData (Id a) where 
  parseUrlPiece = fmap Id . parseUrlPiece

instance PathPiece (Id a) where 
  fromPathPiece = fmap Id . fromPathPiece
  toPathPiece (Id i) = toPathPiece i 

-- | Type of action that is permited by a REST API
data RESTAction = Read | Write | Create | Delete 
  deriving (Show, Eq, Generic)

-- | Calculate permission labels for rest action
type family RESTPermission (t :: RESTAction) (a :: Symbol) = res | res -> t a where 
  RESTPermission 'Read a = 'PermConcat ('PermLabel "read-") ('PermLabel a)
  RESTPermission 'Write a = 'PermConcat ('PermLabel "write-") ('PermLabel a)
  RESTPermission 'Create a = 'PermConcat ('PermLabel "create-") ('PermLabel a)
  RESTPermission 'Delete a = 'PermConcat ('PermLabel "delete-") ('PermLabel a)

-- | Corresponding data type with patch data for 'a'
type family PatchRec a

-- | Generation of REST-full API
type RESTFull a (aname :: Symbol) = RESTFullWith '[ 'GET, 'POST, 'PUT, 'PATCH, 'DELETE] a aname

type family RESTEndpoint (action :: StdMethod) a (aname :: Symbol) = res | res -> action a aname where 
  RESTEndpoint 'GET a aname = Capture "id" (Id a)
    :> TokenHeader '[RESTPermission 'Read aname]
    :> Get '[JSON] a

  RESTEndpoint 'POST a aname = ReqBody '[JSON] a 
    :> TokenHeader '[RESTPermission 'Create aname]
    :> Post '[JSON] (OnlyId (Id a))

  RESTEndpoint 'PUT a aname = Capture "id" (Id a)
    :> ReqBody '[JSON] a 
    :> TokenHeader '[RESTPermission 'Write aname]
    :> Put '[JSON] Unit 

  RESTEndpoint 'PATCH a aname = Capture "id" (Id a)
    :> ReqBody '[JSON] (PatchRec a)
    :> TokenHeader '[RESTPermission 'Write aname]
    :> Patch '[JSON] Unit

  RESTEndpoint 'DELETE a aname = Capture "id" (Id a)
    :> TokenHeader '[RESTPermission 'Delete aname]
    :> Delete '[JSON] Unit

 -- | Generation of partial REST API
type family RESTFullWith (actions :: [StdMethod]) a (aname :: Symbol) where
  RESTFullWith '[action] a aname = RESTEndpoint action a aname
  RESTFullWith (action ': actions) a aname = RESTEndpoint action a aname :<|> RESTFullWith actions a aname 
