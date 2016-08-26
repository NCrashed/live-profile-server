module Profile.Live.Server.Client.Connection(
    connGet
  , connPost
  , connPut
  , connPatch
  , connDelete
  ) where 

import Control.Monad.Trans.Either
import Data.Aeson.Unit 
import Data.Aeson.WithField 
import GHCJS.Marshal
import Servant.API
import Servant.API.Auth.Token
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


(      connGet
  :<|> connPost
  :<|> connPut
  :<|> connPatch
  :<|> connDelete
    ) = client connectionAPI host
  where host = Nothing -- Just $ BaseUrl Http "localhost" 3000

instance ToJSVal ConnectionPatch where 
  toJSVal = toJSVal_aeson