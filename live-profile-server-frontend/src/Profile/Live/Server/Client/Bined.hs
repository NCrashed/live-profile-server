{-|
Module      : Profile.Live.Server.Client.Bined
Description : Drawing of bined graph
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE RecursiveDo #-}
module Profile.Live.Server.Client.Bined(
    BinsLine(..)
  , TimeLine(..)
  , binedDiagram
  , binnedDiagramDebugWidget
  ) where 

import Diagrams.Backend.Reflex as DR
import Diagrams.Prelude as D
import Reflex as R
import Reflex.Dom as R
import Text.Printf

import qualified Data.Vector as V

data BinsLine = BinsLine {
  binsName :: String
, binsColor :: Colour Double 
, binsOffset :: Int 
, binsValues :: V.Vector Double
}

mkBins :: BinsLine -> Diagram B
mkBins BinsLine{..} = label ||| offset' ||| bins
  where 
  label = font "Georgia, serif" (fontSize 3 (D.text binsName)) ||| strutX 1
  offset' = strutX (fromIntegral binsOffset)
  bins = hcat $ V.toList $ mkBin <$> binsValues
  mkBin d = square 1 # fc (blend d binsColor white)

-- | Info about time line
data TimeLine = TimeLine {
  timeStart :: Float 
, timeEnd :: Float 
, timeLabelGap :: Int   
, timeBinsCount :: Int
}

-- | Get time value at start of given bin
timeLineVal :: TimeLine -> Int -> Float 
timeLineVal TimeLine{..} i = timeStart 
  + (fromIntegral i) * (timeEnd - timeStart) / fromIntegral timeBinsCount

-- | Draw timeline
mkTimeLine :: TimeLine 
  -> Diagram B
mkTimeLine tl@TimeLine{..} = strutX 2 ||| (tline <> ticks <> labels) ||| strutX 1
  where 
  n = timeBinsCount
  tline = fromOffsets [V2 (fromIntegral n) 0]
  isBigTick i = i `mod` timeLabelGap == 0
  tick i v = fromVertices $ P <$> [V2 (fromIntegral i) 0, V2 (fromIntegral i) (-v) ]
  ticks = mconcat [ if isBigTick i then tick i 0.2 else tick i 0.1 | i <- [0 .. n]]
  mkLabel i = D.text (printf "%.2f" (timeLineVal tl i) ++ "s") 
    # moveTo (P $ V2 (fromIntegral i) 0.1)
    # fontSize 2 
  labels = mconcat [ mkLabel i | i <- [0 .. n], isBigTick i]

binedDiagram :: P2 Double -> Diagram B
binedDiagram _ = frame 0.1 (tline' === strutY 0.5 === bins')
  where 
  tline' = mkTimeLine tline
  tline = TimeLine 0 10 5 maxn
  bins' = vcat $ mkBins <$> bins
  bins = [
      BinsLine "GC" red 0 $ V.fromList $ take 50 $ cycle sample0
    , BinsLine "1" blue 0 $ V.fromList $ take 50 $ cycle sample1
    , BinsLine "2" blue 7 $ V.fromList $ take 10 $ cycle sample2
    ]
  sample0 = [0.0, 0.1, 0.1, 0.2, 0.0, 0.5, 0.8, 0.3, 0.1, 0.2, 0.0, 0.1, 0.1]
  sample1 = [0.5, 0.4, 0.3, 0.5, 0.9, 1.0, 1.0, 0.1, 0.4, 0.5, 0.7, 0.9, 0.4]
  sample2 = [0.5, 0.9, 1.0, 1.0, 0.1, 0.4, 0.5]
  maxn = maximum $ V.length . binsValues  <$> bins

binnedDiagramDebugWidget :: MonadWidget t m => m ()
binnedDiagramDebugWidget = mdo
  -- svgDyn :: Dynamic t (m (DiaEv Any))
  svgDyn <- mapDyn (reflexDia $ def & sizeSpec .~ dims2D 1920 1000) diaDyn
  -- pos :: Event (P2 Double)
  pos <- switchPromptly never <$> fmap diaMousemovePos =<< dyn svgDyn
  -- diaDyn :: Dynamic (Diagram B)
  diaDyn <- holdDyn (binedDiagram . p2 $ (0, -1000)) (binedDiagram <$> pos)
  return ()
