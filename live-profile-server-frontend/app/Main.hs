{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Backend.Reflex as DR
import Diagrams.Prelude as D
import Reflex as R
import Reflex.Dom as R
import Text.Printf

import qualified Data.Vector as V

-- timeline :: Float -- ^ Starting time
--   -> Float -- ^ Ending time
--   -> Int -- ^ Number of bins
--   -> Int -- ^ Gap between labels
--   -> Float -- ^ Bin size
--   -> Picture 
-- timeline t1 t2 n gap size = Line 0 0 (size * fromIntegral n) 0
--   <> ticks 
--   where 
--   tick i s = Line (fromIntegral i * size) 0 (fromIntegral i * size) s
--   isBigTick i = i `mod` gap == 0
--   ticks = mconcat [ if isBigTick i then tick i 5 else tick i 3 | i <- [0 .. n]]


main :: IO ()
main = mainWidget app

app :: MonadWidget t m => m ()
app = mdo
  -- svgDyn :: Dynamic t (m (DiaEv Any))
  svgDyn <- mapDyn (reflexDia $ def & sizeSpec .~ dims2D 1920 1000) diaDyn
  -- pos :: Event (P2 Double)
  pos <- switchPromptly never <$> fmap diaMousemovePos =<< dyn svgDyn
  -- diaDyn :: Dynamic (Diagram B)
  diaDyn <- holdDyn (mkDia . p2 $ (0, -1000)) (mkDia <$> pos)
  return ()

-- TODO generalize and move into diagrams-lib
constrain :: (InSpace v n a, Enveloped a, HasBasis v, Num n, Ord (v n)) =>
             a -> Point v n -> Point v n
constrain a p = maybe p c $ getCorners box where
  c (l,h) = max l (min h p)
  box = boundingBox a

data BinsLine = BinsLine {
  binsName :: String
, binsColor :: Colour Double 
, binsOffset :: Int 
, binsValues :: V.Vector Double
}

mkBins :: BinsLine -> Diagram B
mkBins BinsLine{..} = label ||| offset ||| bins
  where 
  label = font "Georgia, serif" (fontSize 3 (D.text binsName)) ||| strutX 1
  offset = strutX (fromIntegral binsOffset)
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

mkDia :: P2 Double -> Diagram B
mkDia p = frame 0.1 (tline' === strutY 0.5 === bins')
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

--mkDia :: P2 Double -> Diagram B
--mkDia p = arr <> c <> back where
--  arr = arrowBetween'
--    (def & arrowHead .~ dart & arrowTail .~ quill )
--    origin p'
--  c = moveTo p' $ D.text "Hello" # fc green
--  p' = p --constrain back p
--  back = vcat [ square 1000 # fc cyan, square 1000 # fc yellow ]