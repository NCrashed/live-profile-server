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
  , whenM
  , whenNothing
  , whenJust
  , fromKey
  , toKey
  ) where

import Control.Monad
import Data.Text (Text)
import Database.Persist.Sql 

import qualified Data.Text as T  

-- | Shortcut 'pack . show'
showt :: Show a => a -> Text 
showt = T.pack . show 

-- | Lifted version of 'when'
whenM :: Monad m => m Bool -> m () -> m ()
whenM action f = do 
  b <- action
  when b f 

-- | Run action if 'Maybe' is 'Nothing'
whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing ma f = case ma of 
  Nothing -> f 
  Just _ -> pure ()

-- | Run action if 'Maybe' is 'Just'
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust ma f = maybe (pure ()) f ma

-- | Shortcut to convert sql key
fromKey :: (Integral a, ToBackendKey SqlBackend record) 
  => Key record -> a 
fromKey = fromIntegral . fromSqlKey

-- | Shortcut to convert sql key
toKey :: (Integral a, ToBackendKey SqlBackend record) 
  => a -> Key record 
toKey = toSqlKey . fromIntegral