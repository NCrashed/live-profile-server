{-|
Module      : Profile.Live.Server.Utils.Schema
Description : Helpers for swagger deriving without name prefixes.
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

Common usage:

> data PagedList i a = PagedList {
>   pagedListItems :: ![a] -- ^ Payload
> , pagedListPages :: !Word -- ^ Count of available pages
> } deriving (Generic, Show)
> 
> instance (ToSchema i, ToSchema a) => ToSchema (PagedList i a) where 
>   declareNamedSchema = genericDeclareNamedSchema $
>     schemaOptionsDropPrefix "pagedList"

-}
module Profile.Live.Server.Utils.Schema(
    schemaOptionsDropPrefix
  ) where 

import Data.Char 
import Data.List
import Data.Swagger

-- | Strip given prefix from fields
schemaOptionsDropPrefix :: String -> SchemaOptions
schemaOptionsDropPrefix pr = defaultSchemaOptions {
    fieldLabelModifier = dropPrefix 
  }
  where 
    dropPrefix s = case stripPrefix pr s of 
      Nothing -> s 
      Just s' -> fixCase s' 

    fixCase x = case x of 
      [] -> []
      (c:cs) -> toLower c : cs