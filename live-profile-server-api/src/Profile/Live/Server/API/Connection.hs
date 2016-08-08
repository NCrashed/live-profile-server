module Profile.Live.Server.API.Connection(
    ConnectionAPI
  -- * Data types
  , Connection
  ) where 

import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Text 
import Data.Time 
import Data.Vinyl.Derived
import GHC.Generics 
import GHC.TypeLits 
import Servant.API 
import Servant.API.Auth.Token

-- | Unique id of resource
newtype Id a = Id { unId :: Word } 
  deriving (Show, Eq, Generic)

-- | Type of action that is permited by a REST API
data RESTAction = Read | Write | Create | Delete 
  deriving (Show, Eq, Generic)

-- | Calculate permission labels for rest action
type family RESTPermission (t :: RESTAction) (a :: Symbol) :: PermSymbol where 
  RESTPermission 'Read a = 'PermConcat (PermLabel "read-") (PermLabel a)
  RESTPermission 'Write a = 'PermConcat (PermLabel "write-") (PermLabel a)
  RESTPermission 'Create a = 'PermConcat (PermLabel "create-") (PermLabel a)
  RESTPermission 'Delete a = 'PermConcat (PermLabel "delete-") (PermLabel a)

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

-- | Connection to remote application
type Connection = FieldRec '[
    '("name", Text)
  , '("host", Text)
  , '("port", Word)
  , '("lastUsed", UTCTime)
  ]

type ConnectionAPI = "connection" :> RESTFull Connection "connection"