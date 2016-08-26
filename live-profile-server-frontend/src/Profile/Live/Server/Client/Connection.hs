module Profile.Live.Server.Client.Connection(
    connGet
  , connPost
  , connPut
  , connPatch
  , connDelete
  , connList
  ) where 

import Control.Monad.Trans.Either
import Data.Aeson.Unit 
import Data.Aeson.WithField 
import GHCJS.Marshal
import Servant.API
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Client 

import Profile.Live.Server.API.Connection
import Profile.Live.Server.Client.Async

type ConnPerm s = MToken '[PermConcat (PermLabel s) (PermLabel "connection")]

connGet :: Id Connection
  -> ConnPerm "read-"
  -> EitherT ServantError IO Connection

connPost :: Connection
  -> ConnPerm "create-"
  -> EitherT ServantError IO (OnlyId (Id Connection))

connPut :: Id Connection
  -> Connection
  -> ConnPerm "write-"
  -> EitherT ServantError IO Unit

connPatch :: Id Connection
  -> PatchRec Connection
  -> ConnPerm "write-"
  -> EitherT ServantError IO Unit

connDelete :: Id Connection
  -> ConnPerm "delete-"
  -> EitherT ServantError IO Unit

connList :: Maybe Page 
  -> Maybe PageSize 
  -> MToken' '["read-connection"]
  -> EitherT ServantError IO (PagedList (Id Connection) Connection)

(      (connGet
  :<|> connPost
  :<|> connPut
  :<|> connPatch
  :<|> connDelete)
  :<|> connList
    ) = client connectionAPI Nothing

instance ToJSVal ConnectionPatch where 
  toJSVal = toJSVal_aeson