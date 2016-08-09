{-|
Module      : Servant.API.REST.Derive.Named
Description : User named types
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Servant.API.REST.Derive.Named(
    Named(..)
  -- * Helpers to implement 'Named' instances
  , getTypeableName 
  ) where 

import Data.Proxy
import Data.Typeable 

-- | Named type which name we can get at runtime
class Named a where 
  getName :: forall proxy . proxy a -> String 

-- | Get type name based on typeable instance
getTypeableName :: forall proxy a . Typeable a => proxy a -> String 
getTypeableName _ = show $ typeRep (Proxy :: Proxy a)