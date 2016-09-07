{-|
Module      : Profile.Live.Server.Client.Utils
Description : Helper module with miscs functions
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
module Profile.Live.Server.Client.Utils(
    header
  , danger
  , info
  , centered
  , whenJust
  , periodical
  , whenPrev
  , genId
  , widgetHoldEvent
  , widgetHoldEvent'
  , foldEvent
  , approxEq
  , traceEventWidget
  , traceDynWidget
  ) where 

import Control.Monad 
import Control.Monad.IO.Class
import Data.IORef 
import Data.Time 
import Reflex
import Reflex.Dom 
import System.IO.Unsafe 

-- | Helper to display centered header
header :: MonadWidget t m => String -> m ()
header = elAttr "h1" [("style", "text-align: center;")] . text 

-- | Helper to dislpay text in red well
danger :: MonadWidget t m => String -> m ()
danger = elClass "div" "alert alert-danger" . text 

-- | Helper to dislpay text in blue well
info :: MonadWidget t m => String -> m ()
info = elClass "div" "alert alert-info" . text 

-- | Create wrapper div that is centered
centered :: MonadWidget t m => m a -> m a 
centered w = elAttr "div" [("style", "text-align: center;")] $ 
  elAttr "div" [("style", "display: inline-block")] w

-- | Perform action only when value contains 'Just'
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a 

-- | Emit event periodical
periodical :: MonadWidget t m => NominalDiffTime -> m (Event t ())
periodical dt = do 
  t <- liftIO getCurrentTime
  tickE <- tickLossy dt t
  return $ fmap (const ()) tickE

-- | Fires when the predicated returns 'True' on previous and current values of event
whenPrev :: MonadWidget t m => (a -> a -> Bool) -> Event t a -> m (Event t a)
whenPrev f ea = do 
  bDyn <- foldDyn accum (Nothing, False) ea
  return $ fmapMaybe filterEv $ updated bDyn
  where 
  accum !a (Nothing, _) = (Just a, True)
  accum !a (Just a', _) = let v = f a' a in v `seq` (Just a, v)

  filterEv (Just a, True) = Just a
  filterEv _ = Nothing 

-- | Reference to global counter for unique id generation
globalIdRef :: IORef Int 
globalIdRef = unsafePerformIO $ newIORef 0
{-# NOINLINE globalIdRef #-}

-- | Generate unique ids
genId :: MonadIO m => m Int 
genId = liftIO $ do 
  i <- readIORef globalIdRef 
  modifyIORef' globalIdRef (+1)
  return i 

-- | Wrapper around 'widgetHold' for widgets that produces events at output
widgetHoldEvent :: MonadWidget t m => m (Event t a) -> Event t (m (Event t a)) -> m (Event t a)
widgetHoldEvent initW we = do 
  dynRes <- widgetHold initW we 
  return $ switchPromptlyDyn dynRes 

-- | Wrapper around 'widgetHold' for widgets that produces events at output
widgetHoldEvent' :: MonadWidget t m => Event t (m (Event t a)) -> m (Event t a)
widgetHoldEvent' we = do 
  dynRes <- widgetHold (return never) we 
  return $ switchPromptlyDyn dynRes 

-- | Wrapper around 'foldDyn' to fold over events
foldEvent :: MonadWidget t m => (a -> b -> b) -> b -> Event t a -> m (Event t b)
foldEvent f b0 e = do 
  dynB <- foldDyn f b0 e 
  return $ updated dynB 

-- | Approximately equality for floatings
approxEq :: (Num a, Ord a, Fractional a) => a -> a -> Bool 
approxEq a b = abs (a - b) < 0.00001

-- | Display contents of value in info panel
traceEventWidget :: (Show a, MonadWidget t m) => Event t a -> m ()
traceEventWidget e = void . widgetHold (pure ()) $ info . show <$> e

-- | Display contents of value in info panel
traceDynWidget :: (Show a, MonadWidget t m) => Dynamic t a -> m ()
traceDynWidget d = do
  d' <- mapDyn (info . show) d 
  void . dyn $ d'