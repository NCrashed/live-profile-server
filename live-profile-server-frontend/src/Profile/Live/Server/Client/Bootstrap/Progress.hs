{-|
Module      : Profile.Live.Server.Client.Bootstrap.Progress
Description : Bootstrap widgets for progress bars
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FunctionalDependencies #-}
module Profile.Live.Server.Client.Bootstrap.Progress(
  -- * Configuration
    ProgressStyle(..)
  , ProgressConfig(..)
  , HasMinValue(..)
  , HasMaxValue(..)
  , HasNowValue(..)
  , HasStyle(..)
  , defaultProgressConfig
  , constPercentProgress
  -- * Widget
  , ProgressBar(..)
  , HasNowValue(..)
  , progressBar
  , debugProgressBar
  ) where 

import Control.Lens 
import Control.Monad.IO.Class
import Data.Monoid 
import Data.Time 
import GHC.Generics 
import Reflex
import Reflex.Dom 
import Text.Printf 

-- | Defines visual style of progress bar
data ProgressStyle =
    ProgressSuccess -- ^ Green
  | ProgressInfo -- ^ Blue
  | ProgressWarning -- ^ Yellow
  | ProgressDanger -- ^ Red
  deriving (Eq, Show, Read, Generic)

-- | Render progress bar style
renderProgressStyle :: ProgressStyle -> String 
renderProgressStyle s = case s of 
  ProgressSuccess -> "progress-bar-success"
  ProgressInfo -> "progress-bar-info"
  ProgressWarning -> "progress-bar-warning"
  ProgressDanger -> "progress-bar-danger"

-- | Configuration of progress bar widget
data ProgressConfig t a = ProgressConfig {
  _progressConfigMinValue :: a -- ^ Maximum value
, _progressConfigMaxValue :: a -- ^ Minimum value
, _progressConfigNowValue :: Dynamic t a -- ^ Input value of progress bar
, _progressConfigStyle :: ProgressStyle -- ^ Color style of progress bar
, _progressConfigStriped :: Bool -- ^ Render striped effect
, _progressConfigDisplayValue :: (a -> String) -- ^ User specified render of label
} deriving (Generic)

$(makeFields ''ProgressConfig)

-- | Default config of progress bar
defaultProgressConfig :: (Reflex t, Show a) => a -- ^ Minimum value
  -> a -- ^ Maximum value
  -> ProgressConfig t a 
defaultProgressConfig minVal maxVal = ProgressConfig {
    _progressConfigMinValue = minVal
  , _progressConfigMaxValue = maxVal 
  , _progressConfigNowValue = constDyn minVal 
  , _progressConfigStyle = ProgressInfo
  , _progressConfigStriped = True
  , _progressConfigDisplayValue = show 
  }

-- | Output of progress bar widget
data ProgressBar t a = ProgressBar {
  _progressBarNowValue :: Event t a   
} deriving (Generic)

$(makeFields ''ProgressBar)

-- | Widget with progress bar
--
-- Renders some like of:
-- @
-- <div class="progress">
--   <div class="progress-bar" role="progressbar" aria-valuenow="0" aria-valuemin="0" aria-valuemax="100" style="min-width: 2em; width: 10%">
--     10%
--   </div>
-- </div>
-- @
progressBar :: (MonadWidget t m, Show a, Num a, Fractional a, Ord a) 
  => ProgressConfig t a -> m (ProgressBar t a)
progressBar ProgressConfig{..} = elClass "div" "progress" $ do
  attrs <- mapDyn makeAttrs _progressConfigNowValue
  elDynAttr "div" attrs $ do
    ea <- dyn =<< mapDyn makePercent _progressConfigNowValue
    return ProgressBar {
        _progressBarNowValue = ea
      }
  where 
  makePercent v = do 
    text $ _progressConfigDisplayValue v 
    return v

  makeAttrs v = [
      ("class", "progress-bar " 
        ++ renderProgressStyle _progressConfigStyle
        ++ if _progressConfigStriped then "progress-bar-striped" else "")
    , ("role", "progressbar")
    , ("aria-valuenow", show v)
    , ("aria-valuemin", show _progressConfigMinValue)
    , ("aria-valuemax", show _progressConfigMaxValue)
    , ("style", "min-width: 2em; width: " ++ show dv' ++ "%")
    ]
    where 
    dv = (v - _progressConfigMinValue) / (_progressConfigMaxValue - _progressConfigMinValue)
    dv' = 100 * max 0 dv

-- | Showcase for progress bar
debugProgressBar :: MonadWidget t m => m ()
debugProgressBar = do 
  t <- liftIO getCurrentTime
  tickE <- tickLossy (fromIntegral (1 :: Int)) t
  tD <- foldDyn (\_ a -> a + 0.1) (0 :: Double) tickE 
  valD <- mapDyn (abs . sin) tD  
  _ <- progressBar $ ProgressConfig 0 1.0 valD ProgressInfo True show2f
  return ()
  where
  show2f = printf "%.2f"

-- | Special config for percentage static progress bar
constPercentProgress :: (Reflex t, Num a, Fractional a, PrintfArg a)
  => a -- ^ Current value
  -> ProgressStyle -- ^ Color style
  -> Bool -- ^ Striped render
  -> ProgressConfig t a
constPercentProgress a style striped = ProgressConfig 0 1.0 
  (constDyn a) style striped show2f
  where
  show2f = (<> "%") . printf "%.2f" . (*100)