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
  , HasRESTDB(..)
  , StorableResource(..)
  , HasRESTServer(..)
  ) where 

import Control.Monad.Except 
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Monoid 
import Data.Proxy 
import Database.Persist.Sql 
import GHC.TypeLits 
import Servant.API 
import Servant.API.Auth.Token 
import Servant.API.REST.Derive
import Servant.Server 
import Servant.Server.Auth.Token

import qualified Data.ByteString.Lazy.Char8 as B

-- | Shortcut for server that handles REST API that was geerated by 'RESTFull' type
type RESTServer a aname m = ServerT (RESTFull a aname) m

-- | Defines server handlers that has access to RDBMS
class HasRESTDB m where 
  runRESTDB :: SqlPersistT IO a -> m a

-- | Operations for 'a' to read/insert/update/delete in RDBMS
class StorableResource a where 
  -- | Reading a resource by id
  readResource :: Id a -> SqlPersistT IO (Maybe a)
  -- | Creation of resource
  insertResource :: a -> SqlPersistT IO (Id a)
  -- | Full update of resource
  replaceResource :: Id a -> a -> SqlPersistT IO ()
  -- | Partial update of resource
  patchResource :: Id a -> PatchRec a -> SqlPersistT IO ()
  -- | Cascade deletion of resource
  deleteResource :: Id a -> SqlPersistT IO ()

-- | Typeclass that derives server implementation
class HasRESTServer a aname m where 
  restServer :: ServerT (RESTFull a aname) m

instance (KnownSymbol aname, HasRESTDB m, StorableResource a, MonadError ServantErr m, AuthMonad m) 
  => HasRESTServer a aname m where 
  restServer = 
         getRes
    :<|> postRes
    :<|> putRes
    :<|> patchRes 
    :<|> deleteRes
    where 
    aname = B.pack $ symbolVal (Proxy :: Proxy aname)

    getRes :: Id a 
      -> MToken '[RESTPermission 'Read aname]
      -> m a 
    getRes i token = do 
      guardAuthToken token 
      ma <- runRESTDB $ readResource i
      maybe (throwError $ err404 { errBody = "Cannot find resource " <> aname })
        return ma 

    postRes :: a 
      -> MToken '[RESTPermission 'Create aname]
      -> m (OnlyId (Id a))
    postRes a token = do 
      guardAuthToken token 
      i <- runRESTDB $ insertResource a 
      return $ OnlyField i 

    putRes :: Id a -> a 
      -> MToken '[RESTPermission 'Write aname]
      -> m Unit 
    putRes i a token = do
      guardAuthToken token 
      runRESTDB $ replaceResource i a 
      return Unit

    patchRes :: Id a -> PatchRec a 
      -> MToken '[RESTPermission 'Write aname]
      -> m Unit 
    patchRes i pa token = do 
      guardAuthToken token 
      runRESTDB $ patchResource i pa 
      return Unit

    deleteRes :: Id a 
      -> MToken '[RESTPermission 'Delete aname]
      -> m Unit
    deleteRes i token = do 
      guardAuthToken token 
      runRESTDB $ deleteResource i 
      return Unit