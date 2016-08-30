{-|
Module      : Profile.Live.Server.Utils.DeriveJson
Description : Helpers for aeson deriving without name prefixes.
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
> $(deriveJSON (derivePrefix "pagedList") ''PagedList)

It will marshal in/from the following json struct:

> { "items": [], "pages": 0 }

-}
module Profile.Live.Server.Utils.DeriveJson(
    derivePrefix
  , deriveJSON
  , defaultOptions
  ) where 

import Data.Aeson.TH               as TH
import Data.Char                   as C 
import Data.Text (Text)

import qualified Data.Text         as T

-- | For aeson deriving, drop prefix t and map to lower
derivePrefix :: Text -> TH.Options
derivePrefix t = defaultOptions {
    fieldLabelModifier = mapFirst C.toLower . drop (T.length t) 
  , constructorTagModifier = camelCaseToDash
  }

mapFirst :: (a -> a) -> [a] -> [a]
mapFirst f cs = case cs of 
  [] -> []
  (x:xs) -> f x : xs  

camelCaseToDash :: String -> String
camelCaseToDash = foldr go "" . mapFirst C.toLower
  where 
  go c acc = if C.isUpper c 
    then '_' : C.toLower c : acc
    else c : acc 