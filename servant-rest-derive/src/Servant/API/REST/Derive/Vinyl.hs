{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Servant.API.REST.Derive.Vinyl
Description : Utilities for vinyl records
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.REST.Derive.Vinyl(
    FieldsSchema(..)
  , ToVinylSchema(..)
  , VinylPatchFields
  , VinylPatch
  ) where 

import Control.Lens
import Control.Monad (mzero)
import Data.Aeson as A
import Data.Aeson.Types
import Data.Proxy
import Data.Swagger
import Data.Swagger.Declare
import Data.Text 
import Data.Vinyl.Core
import Data.Vinyl.Derived
import Data.Vinyl.Lens
import GHC.TypeLits 

import Servant.API.REST.Derive.Named 
import Servant.API.REST.Derive.Patch

import qualified Data.HashMap.Strict as H

class FieldsSchema (fields :: [(Symbol, *)]) where 
  fieldsSchemaProperties :: forall proxy . proxy fields -> H.HashMap Text (Referenced Schema)
  fieldsSchemaRequired :: forall proxy . proxy fields -> [ParamName]

instance FieldsSchema '[] where 
  fieldsSchemaProperties _ = mempty
  fieldsSchemaRequired _ = mempty

instance {-# OVERLAPPABLE #-} (KnownSymbol n, ToSchema a, FieldsSchema fs) => FieldsSchema ('(n, a) ': fs) where 
  fieldsSchemaProperties _ = H.insert n (Inline sr) $ fieldsSchemaProperties (Proxy :: Proxy fs)
    where  
    n = pack $ symbolVal (Proxy :: Proxy n)
    sr = toSchema (Proxy :: Proxy a) 
  fieldsSchemaRequired _ = n : fieldsSchemaRequired (Proxy :: Proxy fs)
    where  
    n = pack $ symbolVal (Proxy :: Proxy n)

instance {-# OVERLAPPING #-} (KnownSymbol n, ToSchema a, FieldsSchema fs) => FieldsSchema ('(n, Maybe a) ': fs) where 
  fieldsSchemaProperties _ = H.insert n (Inline sr) $ fieldsSchemaProperties (Proxy :: Proxy fs)
    where  
    n = pack $ symbolVal (Proxy :: Proxy n)
    sr = toSchema (Proxy :: Proxy (Maybe a)) 
  fieldsSchemaRequired _ = fieldsSchemaRequired (Proxy :: Proxy fs)

instance (Named (FieldRec fields)
        , FieldsSchema fields) 
    => ToSchema (FieldRec fields) where 
  declareNamedSchema p = declareVinylSchema (pack $ getName p) p

class ToJSONProps a where 
  toJSONProps :: a -> [Pair]

instance ToJSONProps (FieldRec '[]) where 
  toJSONProps _ = []

instance (KnownSymbol n, ToJSON a, ToJSONProps (FieldRec fs)) => ToJSONProps (FieldRec ('(n, a) ': fs)) where
  toJSONProps (Field a :& as) = (n A..= toJSON a) : toJSONProps as 
    where 
    n = pack $ symbolVal (Proxy :: Proxy n)

instance ToJSONProps (FieldRec fields) => ToJSON (FieldRec fields) where 
  toJSON rc = object $ toJSONProps rc

instance FromJSON (FieldRec '[]) where 
  parseJSON _ = pure RNil

instance (KnownSymbol n, FromJSON a, FromJSON (FieldRec fs)) => FromJSON (FieldRec ('(n, a) ': fs)) where 
  parseJSON js@(Object o) = do 
    let n = pack $ symbolVal (Proxy :: Proxy n)
    a <- o .: n 
    as <- parseJSON js
    return $ Field a :& as
  parseJSON _ = mzero

-- | Derive vinyl record fields that can be used as partial update payload for given fields record
type family VinylPatchFields (fields :: [(Symbol, *)]) :: [(Symbol, *)] where
  VinylPatchFields '[] = '[]
  VinylPatchFields ('(n, Maybe a) ': as) = '(n, NullablePatch a) ': VinylPatchFields as
  VinylPatchFields ('(n, a) ': as) = '(n, Maybe a) ': VinylPatchFields as

-- | Wrapper around 'VinylPatchFields' to produce corresponding patch record for a vinyl record
type family VinylPatch a where 
  VinylPatch (FieldRec fields) = FieldRec (VinylPatchFields fields)

-- | Helper to derive 'ToSchema' instances for wrappers around vinyl records
--
-- @
-- | Correspoinding patch record
-- newtype ConnectionPatch = ConnectionPatch { unConnectionPatch :: VinylPatch Connection }
--   deriving (ToJSON, FromJSON, Show)
-- 
-- instance ToSchema ConnectionPatch where 
--   declareNamedSchema _ = declareVinylSchema "ConnectionPatch" (Proxy :: Proxy (VinylPatch Connection))
-- @
class ToVinylSchema a where 
  declareVinylSchema :: forall proxy . Text -> proxy a -> Declare (Definitions Schema) NamedSchema

instance FieldsSchema fields => ToVinylSchema (FieldRec fields) where 
  declareVinylSchema nm _ = do 
    let props = fieldsSchemaProperties (Proxy :: Proxy fields)
        reqs = fieldsSchemaRequired (Proxy :: Proxy fields)
    return $ NamedSchema (Just nm) $ mempty 
      & type_ .~ SwaggerObject
      & properties .~ props 
      & required .~ reqs


instance Patchable (FieldRec fields) (FieldRec '[]) where
  applyPatch a _ = a 
  {-# INLINE applyPatch #-}

instance (
    KnownSymbol n
  , RElem '(n, a) fields i
  , Patchable (FieldRec fields) (FieldRec fs)
  ) => Patchable (FieldRec fields) (FieldRec ('(n, Maybe a) ': fs)) where 
  applyPatch a (b :& bs) = applyPatch a' bs  
    where 
    a' = case b of 
      Field Nothing  -> a
      Field (Just v) -> setter v a
    setter :: a -> FieldRec fields -> FieldRec fields
    setter v = rput (Field v :: ElField '(n, a))
  {-# INLINE applyPatch #-}

instance (
    KnownSymbol n
  , RElem '(n, Maybe a) fields i
  , Patchable (FieldRec fields) (FieldRec fs)
  ) => Patchable (FieldRec fields) (FieldRec ('(n, NullablePatch a) ': fs)) where 
  applyPatch a (b :& bs) = applyPatch a' bs  
    where 
    a' = case b of
      Field NoPatch -> a
      Field NullifyPatch -> setter Nothing a
      Field (ValuePatch v) -> setter (Just v) a
    setter :: Maybe a -> FieldRec fields -> FieldRec fields
    setter v = rput (Field v :: ElField '(n, Maybe a))
  {-# INLINE applyPatch #-}