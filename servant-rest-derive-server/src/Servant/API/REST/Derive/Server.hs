{-|
Module      : Servant.API.REST.Derive.Server
Description : Derive server side implementation for REST API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.REST.Derive.Server(
    RESTServer
  , HasRESTServer(..)
  ) where 

import Data.Aeson.Unit
import Data.Aeson.WithField
import Servant.API 
import Servant.API.Auth.Token 
import Servant.API.REST.Derive
import Servant.Server 
--import Servant.Server.Auth.Token 

type RESTServer a aname m = ServerT (RESTFull a aname) m

-- | Typeclass that derives server implementation
class HasRESTServer a aname m where 
  restServer :: ServerT (RESTFull a aname) m

instance HasRESTServer a aname m where 
  restServer = 
         getRes
    :<|> postRes
    :<|> putRes
    :<|> patchRes 
    :<|> deleteRes
    where 
    getRes :: Id a 
      -> MToken '[RESTPermission 'Read aname]
      -> m a 
    getRes = undefined 

    postRes :: a 
      -> MToken '[RESTPermission 'Create aname]
      -> m (OnlyId (Id a))
    postRes = undefined

    putRes :: Id a 
      -> a 
      -> MToken '[RESTPermission 'Write aname]
      -> m Unit 
    putRes = undefined

    patchRes :: Id a
      -> PatchRec a 
      -> MToken '[RESTPermission 'Write aname]
      -> m Unit 
    patchRes = undefined

    deleteRes :: Id a 
      -> MToken '[RESTPermission 'Delete aname]
      -> m Unit
    deleteRes = undefined