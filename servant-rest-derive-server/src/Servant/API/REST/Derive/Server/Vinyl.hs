{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Servant.API.REST.Derive.Server.Vinyl
Description : Helpers to derive REST server for vinyl records
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.REST.Derive.Server.Vinyl(
    DeriveEntityFields(..)
  , ToVinylPersistFields(..)
  , FromVinylPersistValues(..)
  , EntityField(DBFieldId, DBField)
  , Key(VKey)
  ) where

import Cases
import Data.Aeson
import Data.Bifunctor
import Data.Monoid 
import Data.Proxy 
import Data.Text (Text, pack, unpack)
import Data.Typeable 
import Data.Vinyl.Core 
import Data.Vinyl.Derived 
import Data.Vinyl.Lens
import Database.Persist
import Database.Persist.Sql
import GHC.TypeLits 
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Named
import Web.HttpApiData
import Web.PathPieces

-- Here the snippet with dumping TH persistent splines
--import Database.Persist.Quasi
--import Database.Persist.TH
--import Language.Haskell.TH
--import Data.Text 
--
-- $(do 
--  let fields = $(persistFileWith lowerCaseSettings "test.persist")
--  reportError . pprint =<< mkPersist sqlSettings fields
--  return []
--  )

-- | Helper for defining fields definitions of persistent entity
class DeriveEntityFields (fields :: [(Symbol, *)]) where 
  deriveEntityFields :: forall proxy . proxy fields -> [FieldDef]

instance DeriveEntityFields '[] where 
  deriveEntityFields _ = []

instance (KnownSymbol n, Typeable a, PersistFieldSql a, DeriveEntityFields fs) => DeriveEntityFields ('(n, a) ': fs) where 
  deriveEntityFields _ = f : deriveEntityFields (Proxy :: Proxy fs)
    where 
    n = pack $ symbolVal (Proxy :: Proxy n)
    as = pack . show $ typeRep (Proxy :: Proxy a)
    f = FieldDef {
        fieldHaskell = HaskellName n
      , fieldDB = DBName . snakify $ n
      , fieldType = FTTypeCon Nothing as
      , fieldSqlType = sqlType (Proxy :: Proxy a)
      , fieldAttrs = []
      , fieldStrict = True
      , fieldReference = NoReference
      }

-- | Helper to wrap vinyl record into 'SomePersistField'
class ToVinylPersistFields a where 
  toVinylPersistFields :: a -> [SomePersistField]

instance ToVinylPersistFields (FieldRec '[]) where 
  toVinylPersistFields _ = []

instance (PersistField a, ToVinylPersistFields (FieldRec as)) => ToVinylPersistFields (FieldRec ('(n, a) ': as)) where 
  toVinylPersistFields (Field a :& as) = SomePersistField a : toVinylPersistFields as

-- | Helper to read vinyl record from persistent values
class FromVinylPersistValues a where 
  fromVinylPersistValues :: [PersistValue] -> Either Text a

instance FromVinylPersistValues (FieldRec '[]) where 
  fromVinylPersistValues _ = Right RNil 

instance (KnownSymbol n, PersistField a, FromVinylPersistValues (FieldRec as)) => FromVinylPersistValues (FieldRec ('(n, a) ': as)) where 
  fromVinylPersistValues vs = case vs of
    [] -> Left $ "expected a value for '" <> n <> "' field"
    (v:vs') -> do 
      a <- first fieldError . fromPersistValue $ v 
      as <- fromVinylPersistValues vs'
      return $ Field a :& as
    where 
      n = pack $ symbolVal (Proxy :: Proxy n)
      fieldError err = "field " <> n <> ": " <> err

--class VinylRuntimeGetter fields a where 
--  vinylRuntimeGetter :: String -> FieldRec fields -> Maybe a 

-- | Helper proof that 'a' symbol is within 'as'
type family WithinFields (a :: (Symbol, *)) (as :: [(Symbol, *)]) :: Bool where 
  WithinFields a '[] = 'False 
  WithinFields a (a ': as) = 'True 
  WithinFields a (b ': as) = WithinFields a as

instance (Named (FieldRec fields)
        , DeriveEntityFields fields
        , ToVinylPersistFields (FieldRec fields)
        , FromVinylPersistValues (FieldRec fields)
        ) => PersistEntity (FieldRec fields) where 

  data Key (FieldRec fields) = VKey { unVKey :: Id (FieldRec fields) }
    deriving (Eq, Show, Read, Ord)

  data EntityField (FieldRec fields) typ where 
    DBFieldId :: EntityField (FieldRec fields) (Key (FieldRec fields))
    DBField :: ('(n, a) ~ field, KnownSymbol n, RElem field fields i) 
      => Proxy field -> EntityField (FieldRec fields) a

  data Unique (FieldRec fields)

  keyToValues = pure . toPersistValue . unId . unVKey
  keyFromValues vs = case vs of 
    [] -> Left "Expected value for id"
    (v:_) -> fmap (VKey . Id) . fromPersistValue $ v

  entityDef _ = EntityDef {
      entityHaskell = HaskellName name
    , entityDB = DBName . snakify $ name
    , entityId = entityIdField name
    , entityAttrs = []
    , entityFields = deriveEntityFields (Proxy :: Proxy fields)
    , entityUniques = []
    , entityForeigns = []
    , entityDerives = []
    , entityExtra = mempty
    , entitySum = False 
    }
    where 
    name = pack $ getName (Proxy :: Proxy (FieldRec fields))

  toPersistFields = toVinylPersistFields
  fromPersistValues = fromVinylPersistValues

  persistUniqueToFieldNames _ = error "Degenerate case, should never happen"
  persistUniqueToValues _ = error "Degenerate case, should never happen"
  persistUniqueKeys _ = []

  persistFieldDef f = case f of 
    DBFieldId -> entityIdField name 
    DBField (_ :: Proxy '(n, a)) -> let
      n = pack $ symbolVal (Proxy :: Proxy n)
      in case filter (hasSameName n) fields of 
        [] -> error . unpack $ "Unknown field '" <> n <> "' for type " <> name
        (x:_) -> x
    where
    fields = deriveEntityFields (Proxy :: Proxy fields)
    name = pack $ getName (Proxy :: Proxy (FieldRec fields))
    hasSameName n FieldDef{..} = let 
      HaskellName n' = fieldHaskell 
      in n' == n 

  type PersistEntityBackend (FieldRec fields) = SqlBackend

  persistIdField = DBFieldId 

  fieldLens f = case f of 
    DBFieldId -> lensPTH entityKey $ \(Entity _ v) i -> Entity i v
    DBField (pn :: Proxy '(n, a)) -> lensPTH (getter . entityVal) $ \(Entity i v) a -> Entity i (setter a v)
      where 
      getter = getField . rget pn
      setter v = rput (Field v :: ElField '(n, a))

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lensPTH :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lensPTH sa sbt afb s = fmap (sbt s) (afb $ sa s)

entityIdField :: Text -> FieldDef
entityIdField n = FieldDef {
    fieldHaskell = HaskellName "Id"
  , fieldDB = DBName "id"
  , fieldType = FTTypeCon Nothing "VKey"
  , fieldSqlType = SqlInt64
  , fieldAttrs = []
  , fieldStrict = True 
  , fieldReference = ForeignRef (HaskellName n) (FTTypeCon Nothing "Word")
  }

instance FromJSON (Key (FieldRec fields)) where 
  parseJSON = fmap VKey . parseJSON

instance ToJSON (Key (FieldRec fields)) where 
  toJSON (VKey i) = toJSON i 

instance PathPiece (Key (FieldRec fields)) where 
  fromPathPiece = fmap VKey . fromPathPiece
  toPathPiece (VKey i) = toPathPiece i 

instance ToHttpApiData (Key (FieldRec fields)) where 
  toUrlPiece (VKey i) = toUrlPiece i 

instance FromHttpApiData (Key (FieldRec fields)) where 
  parseUrlPiece = fmap VKey . parseUrlPiece

instance PersistFieldSql (Key (FieldRec fields)) where 
  sqlType _ = sqlType (Proxy :: Proxy Word)

instance PersistField (Key (FieldRec fields)) where 
  toPersistValue (VKey (Id w)) = toPersistValue w 
  fromPersistValue v = case v of 
    PersistInt64 i -> Right . VKey . Id . fromIntegral $ i
    _ -> Left "Expected Int64 value for key"

