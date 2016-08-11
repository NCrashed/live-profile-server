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
  ) where 

import Control.Lens
import Control.Monad (mzero)
import Data.Aeson as A
import Data.Aeson.Types
import Data.Proxy
import Data.Swagger
import Data.Text 
import Data.Vinyl.Core
import Data.Vinyl.Derived
import GHC.TypeLits 

import Servant.API.REST.Derive.Named 

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
  declareNamedSchema _ = do 
    let nm = pack $ getName (Proxy :: Proxy (FieldRec fields))
        props = fieldsSchemaProperties (Proxy :: Proxy fields)
        reqs = fieldsSchemaRequired (Proxy :: Proxy fields)
    return $ NamedSchema (Just nm) $ mempty 
      & type_ .~ SwaggerObject
      & properties .~ props 
      & required .~ reqs

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
