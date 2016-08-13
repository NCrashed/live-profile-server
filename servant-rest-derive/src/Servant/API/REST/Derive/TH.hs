{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Servant.API.REST.Derive.Server.TH
Description : Helper to completly remove declaration noise from buisness logic modules
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.REST.Derive.TH(
    declareVinylPatch
  ) where 

import Data.Aeson
import Data.Monoid 
import Data.Proxy 
import Data.Swagger
import Language.Haskell.TH
import Language.Haskell.TH.Syntax 

import Servant.API.REST.Derive
import Servant.API.REST.Derive.Patch
import Servant.API.REST.Derive.Vinyl

-- | Generate the following code:
--
-- @
-- newtype <Record>Patch = <Record>Patch { un<Record>Patch :: VinylPatch <Record> }
--   deriving (ToJSON, FromJSON, Show)
-- 
-- type instance PatchRec <Record> = <Record>Patch
-- 
-- instance ToSchema <Record>Patch where 
--   declareNamedSchema _ = declareVinylSchema "<Record>Patch" (Proxy :: Proxy (VinylPatch <Record>))
-- 
-- instance Patchable <Record> <Record>Patch where 
--   applyPatch a (<Record>Patch b) = applyPatch a b
-- @
declareVinylPatch :: Name -> Q [Dec]
declareVinylPatch recName = do 
  newtypeDec <- newtypeD 
    (pure []) 
    patchRecName 
    []
    Nothing
    (recC patchRecName [
      varBangType patchRecNameField $ bangType (bang noSourceUnpackedness noSourceStrictness) [t| VinylPatch $(conT recName) |]
      ]
    ) 
    (sequence [conT ''Eq, conT ''Show, conT ''ToJSON, conT ''FromJSON])
  
  tinst <- [d| type instance PatchRec $(conT recName) = $(conT patchRecName) |]
  
  toSchemaInst <- [d|
    instance ToSchema $(conT patchRecName) where 
       declareNamedSchema _ = declareVinylSchema $(lift patchRecNameStr) (Proxy :: Proxy (VinylPatch $(conT recName)))
    |]

  patchableInst <- do
    bname <- newName "b"
    [d|
      instance Patchable $(conT recName) $(conT patchRecName) where 
        applyPatch a $(conP patchRecName [varP bname]) = applyPatch a $(varE bname)
      |]
  return $
    [newtypeDec]
    ++ tinst
    ++ toSchemaInst
    ++ patchableInst
  where
  patchRecNameStr = nameBase recName <> "Patch"
  patchRecName = mkName patchRecNameStr
  patchRecNameField = mkName $ "un" <> patchRecNameStr