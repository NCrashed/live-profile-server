{-|
Module      : Profile.Live.Server.Utils
Description : Generic helpers
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Utils(
    showt
  ) where

import Data.Text (Text)

import qualified Data.Text as T  

-- | Shortcut 'pack . show'
showt :: Show a => a -> Text 
showt = T.pack . show 