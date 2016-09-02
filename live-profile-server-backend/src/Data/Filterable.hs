{-|
Module      : Data.Filterable
Description : Generic filter function
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

The module is designed to be imported as qualified:
@
import qualified Data.Filterable as F
@
-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Filterable(
    Filterable(..)
  ) where 

import Data.Hashable (Hashable)
import GHC.Exts (Constraint)
import Prelude as P 

import qualified Data.HashMap.Strict as H 
import qualified Data.Sequence as S 
import qualified Data.Vector as V 
import qualified Data.Vector.Unboxed as VU 

-- | Generic filter overloaded for several containers
class Filterable m a where 
  -- | Container can add custom constraint on elements
  type FilterConstr m a :: Constraint
  type FilterConstr m a = ()

  -- | Standart filter like function that retain elements
  -- only when the predicate returns 'True'
  filter :: FilterConstr m a => (a -> Bool) -> m a -> m a 

instance Filterable [] a where 
  filter = P.filter

instance Filterable S.Seq a where 
  filter = S.filter 

instance Filterable V.Vector a where 
  filter = V.filter 

instance Filterable VU.Vector a where 
  type FilterConstr VU.Vector a = VU.Unbox a
  filter = VU.filter 

instance Filterable (H.HashMap k) a where 
  type FilterConstr (H.HashMap k) a = Hashable a
  filter = H.filter 