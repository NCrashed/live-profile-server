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
  ) where 

import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Proxy 
import Data.Swagger 
import GHC.Generics
import GHC.TypeLits 
import Servant.API 
import Servant.API.Auth.Token 

-- | Unique id of resource
newtype Id a = Id { unId :: Word } 
  deriving (Show, Eq, Generic)

instance ToParamSchema (Id a) where 
  toParamSchema _ = toParamSchema (Proxy :: Proxy Word)

-- | Type of action that is permited by a REST API
data RESTAction = Read | Write | Create | Delete 
  deriving (Show, Eq, Generic)

-- | Calculate permission labels for rest action
type family RESTPermission (t :: RESTAction) (a :: Symbol) :: PermSymbol where 
  RESTPermission 'Read a = 'PermConcat ('PermLabel "read-") ('PermLabel a)
  RESTPermission 'Write a = 'PermConcat ('PermLabel "write-") ('PermLabel a)
  RESTPermission 'Create a = 'PermConcat ('PermLabel "create-") ('PermLabel a)
  RESTPermission 'Delete a = 'PermConcat ('PermLabel "delete-") ('PermLabel a)

-- | Corresponding data type with patch data for 'a'
type family PatchRec a

-- | Generation of REST-full API
type RESTFull a (aname :: Symbol) = 
       Capture "id" (Id a)
    :> TokenHeader '[RESTPermission 'Read aname]
    :> Get '[JSON] a
  :<|> ReqBody '[JSON] a 
    :> TokenHeader '[RESTPermission 'Create aname]
    :> Post '[JSON] (OnlyId a)
  :<|> Capture "id" (Id a)
    :> ReqBody '[JSON] a 
    :> TokenHeader '[RESTPermission 'Write aname]
    :> Put '[JSON] Unit 
  :<|> Capture "id" (Id a)
    :> ReqBody '[JSON] (PatchRec a)
    :> TokenHeader '[RESTPermission 'Write aname]
    :> Patch '[JSON] Unit
  :<|> Capture "id" (Id a)
    :> TokenHeader '[RESTPermission 'Delete aname]
    :> Delete '[JSON] Unit