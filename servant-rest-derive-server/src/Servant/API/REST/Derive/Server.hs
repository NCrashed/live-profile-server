{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , ResourceRead(..)
  , ResourceWrite(..)
  , ResourcePatch(..)
  , ResourceDelete(..)
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
--import Servant.API.Auth.Token 
import Servant.API.REST.Derive
import Servant.Server 
import Servant.Server.Auth.Token

import qualified Data.ByteString.Lazy.Char8 as B

-- | Shortcut for server that handles REST API that was geerated by 'RESTFull' type
type RESTServer a aname m = ServerT (RESTFull a aname) m

-- | Defines server handlers that has access to RDBMS
class HasRESTDB m where 
  runRESTDB :: SqlPersistT IO a -> m a

-- | Operations for 'a' to read in RDBMS
class ResourceRead a where 
  -- | Reading a resource by id
  readResource :: Id a -> SqlPersistT IO (Maybe a)

-- | Operations for 'a' to write into RDBMS
class ResourceWrite a where 
  -- | Creation of resource
  insertResource :: a -> SqlPersistT IO (Id a)
  -- | Full update of resource
  replaceResource :: Id a -> a -> SqlPersistT IO ()

-- | Operations for 'a' for partial update
class ResourcePatch a where 
  -- | Partial update of resource
  patchResource :: Id a -> PatchRec a -> SqlPersistT IO ()

-- | Operations for 'a' to delete resources
class ResourceDelete a where 
  -- | Cascade deletion of resource
  deleteResource :: Id a -> SqlPersistT IO ()

-- | Typeclass that derives server implementation
class HasRESTServer (actions :: [StdMethod]) a aname m where 
  type RestHandler actions a (aname :: Symbol) (m :: * -> *) :: * 

  restServer :: forall proxy1 proxy2 proxy3 proxy4 .
       proxy1 actions 
    -> proxy2 a 
    -> proxy3 aname 
    -> proxy4 m 
    -> RestHandler actions a aname m

instance (
    HasRESTHandler action1 a aname m
  , HasRESTHandler action2 a aname m
  , HasRESTServer actions a aname m 
  ) => HasRESTServer (action1 ': action2 ': actions) a aname m where 
  type RestHandler (action1 ': action2 ': actions) a aname m = 
         ServerT (RESTEndpoint action1 a aname) m 
    :<|> ServerT (RESTEndpoint action2 a aname) m 
    :<|> RestHandler actions a aname m

  restServer _ p2 p3 p4 = handler1 :<|> handler2 :<|> restServer (Proxy :: Proxy actions) p2 p3 p4
    where 
      handler1 = restHandler (Proxy :: Proxy action1) p2 p3 p4
      handler2 = restHandler (Proxy :: Proxy action2) p2 p3 p4

instance (
    HasRESTHandler action a aname m
  ) => HasRESTServer (action ': '[]) a aname m where 
  type RestHandler (action ': '[]) a aname m = ServerT (RESTEndpoint action a aname) m 

  restServer _ p2 p3 p4 = restHandler (Proxy :: Proxy action) p2 p3 p4

-- | Deriving a single REST endpoint for a method
class HasRESTHandler (action :: StdMethod) a aname m where 
  restHandler :: forall proxy1 proxy2 proxy3 proxy4 . 
       proxy1 action 
    -> proxy2 a 
    -> proxy3 aname 
    -> proxy4 m 
    -> ServerT (RESTEndpoint action a aname) m

instance (
    KnownSymbol aname
  , HasRESTDB m
  , ResourceRead a
  , MonadError ServantErr m
  , AuthMonad m
  ) => HasRESTHandler 'GET a aname m where 
  restHandler _ _ _ _ i token = do 
    guardAuthToken token 
    ma <- runRESTDB $ readResource i
    maybe (throwError $ err404 { errBody = "Cannot find resource " <> aname })
      return ma 
    where 
      aname = B.pack $ symbolVal (Proxy :: Proxy aname)

instance (
    KnownSymbol aname
  , HasRESTDB m
  , ResourceWrite a
  , AuthMonad m
  ) => HasRESTHandler 'POST a aname m where 
  restHandler _ _ _ _ a token = do 
    guardAuthToken token 
    i <- runRESTDB $ insertResource a 
    return $ OnlyField i 

instance (
    KnownSymbol aname
  , HasRESTDB m
  , ResourceWrite a
  , AuthMonad m
  ) => HasRESTHandler 'PUT a aname m where 
  restHandler _ _ _ _ i a token = do
    guardAuthToken token 
    runRESTDB $ replaceResource i a 
    return Unit

instance (
    KnownSymbol aname
  , HasRESTDB m
  , ResourcePatch a
  , AuthMonad m
  ) => HasRESTHandler 'PATCH a aname m where 
  restHandler _ _ _ _ i pa token = do 
    guardAuthToken token 
    runRESTDB $ patchResource i pa 
    return Unit

instance (
    KnownSymbol aname
  , HasRESTDB m
  , ResourceDelete a
  , AuthMonad m
  ) => HasRESTHandler 'DELETE a aname m where 
  restHandler _ _ _ _ i token = do 
    guardAuthToken token 
    runRESTDB $ deleteResource i 
    return Unit

instance PersistFieldSql (Id a) where 
  sqlType _ = sqlType (Proxy :: Proxy Word)

instance PersistField (Id a) where 
  toPersistValue (Id w) = toPersistValue w 
  fromPersistValue v = case v of 
    PersistInt64 i -> Right . Id . fromIntegral $ i
    _ -> Left "Expected Int64 value for key"