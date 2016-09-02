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
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Profile.Live.Server.Client.Bined(
  -- * Server API
    getFullBinedGraph
  -- * Widgets
  , binedGraphWidget
  , binnedDiagramDebugWidget
  -- * Drawing of bined graph
  , binedDiagram
  ) where 

import Control.Monad.Trans.Either
import Data.Colour.SRGB.Linear 
import Data.Text (unpack)
import Diagrams.Backend.Reflex as DR
import Diagrams.Prelude as D
import GHCJS.Marshal
import Reflex as R
import Reflex.Dom as R
import Servant.API.Auth.Token
import Servant.Client 
import Text.Printf

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU 
import qualified Data.Text as T 

import Profile.Live.Server.API.Bined
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Bootstrap.Button 
import Profile.Live.Server.Client.Router 
import Profile.Live.Server.Client.Utils 

-- | Get all info required to render bined graph for eventlog
getFullBinedGraph :: EventLogId
  -> Maybe Double -- ^ bin width
  -> Maybe (RGB Double) -- ^ thread line colour
  -> Maybe (RGB Double) -- ^ gc line colour
  -> Maybe (RGB Double) -- ^ custom event colour
  -> MToken' '["bined-graph"] -- ^ authorisation token
  -> EitherT ServantError IO BinedGraph

(      getFullBinedGraph 
  ) = client binedAPI Nothing 

instance ToJSVal BinedGraph where 
  toJSVal = toJSVal_aeson

instance FromJSVal BinedGraph where 
  fromJSVal = fromJSVal_aeson

-- | Widget to display bined graph
binedGraphWidget :: forall t m . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> Maybe (m (Route t m)) -- ^ Possible widget for "Back" button
  -> EventLogId -- ^ Id of eventlog
  -> m (Route t m)
binedGraphWidget tok backW eid = do 
  header "Bined graph"
  (backE, reloadReqE) <- centered $ buttonGroup $ do 
    backE <- blueButton "Back"
    reloadReqE  <- blueButton "Reload"
    return (backE, reloadReqE)

  initialE <- getPostBuild
  let reloadE = leftmost [reloadReqE, initialE]
  graphE <- requestBined reloadE
  _ <- widgetHold (pure ()) $ renderBined <$> graphE
  --binnedDiagramDebugWidget

  return $ maybe (Route never) (\w -> Route $ const w <$> backE) backW
  where 
  renderBined :: BinedGraph -> m ()
  renderBined graph = mdo 
    let dia = binedDiagram graph
    _ <- reflexDia (def & sizeSpec .~ dims2D 1800 900) dia
    return ()

  requestBined :: forall a . Event t a -> m (Event t BinedGraph)
  requestBined ep = simpleRequest ep (const $
    getFullBinedGraph eid Nothing Nothing Nothing Nothing (Just (Token tok)))


-- | Draw single bined line
mkBins :: Double -- ^ Offset for labels
  -> BinLine
  -> Diagram B
mkBins labelOffset BinLine{..} = label ||| offset' ||| bins
  where 
  label = font "Georgia, serif" (fontSize 3 (D.text . unpack $ binLineName)) ||| strutX labelOffset
  offset' = strutX (fromIntegral binLineOffset)
  bins = hcat $ mkBin <$> VU.toList binLineValues
  mkBin d = square 1 # fc (blend d (mkColour binLineColour) white)
  mkColour (RGB r g b) = rgb r g b 

-- | Draw group of bined lines
mkLineGroup :: Double -- ^ Offset for labels 
  -> LineGroup 
  -> Diagram B 
mkLineGroup labelOffset LineGroup{..} 
  | null lineGroupLines = mempty 
  | otherwise = strutY 0.7 === label === strutY 0.5 === bins 
  where 
  labelText = T.toUpper lineGroupName
  label = font "Georgia, serif" (fontSize 3 (D.text . unpack $ labelText))
  bins = vcat $ V.toList $ mkBins labelOffset <$> lineGroupLines

-- | Info about time line
data TimeLine = TimeLine {
  timeStart :: Double 
, timeEnd :: Double 
, timeLabelGap :: Int   
, timeBinsWidth :: Double
}

-- | Extract time line from server data
extractTimeLine :: BinedGraph -> TimeLine 
extractTimeLine BinedGraph{..} = TimeLine {
    timeStart = binedGraphBegin
  , timeEnd = binedGraphEnd
  , timeLabelGap = 5
  , timeBinsWidth = binedGraphBinWidth
  }

-- | Get time value at start of given bin
timeLineVal :: TimeLine -> Int -> Double 
timeLineVal TimeLine{..} i = timeStart + (fromIntegral i) * timeBinsWidth

-- | Draw timeline
mkTimeLine :: TimeLine
  -> Diagram B
mkTimeLine tl@TimeLine{..} = (tline <> ticks <> labels) ||| strutX 2
  where 
  n = ceiling $ (timeEnd - timeStart) / timeBinsWidth
  tline = fromOffsets [V2 (fromIntegral n) 0]
  isBigTick i = i `mod` timeLabelGap == 0
  tick i v = fromVertices $ P <$> [V2 (fromIntegral i) 0, V2 (fromIntegral i) (-v) ]
  ticks = mconcat [ if isBigTick i then tick i 0.2 else tick i 0.1 | i <- [0 .. n]]
  mkLabel i = D.text (printf "%.2f" (timeLineVal tl i) ++ "s") 
    # moveTo (P $ V2 (fromIntegral i) 0.1)
    # fontSize 2 
  labels = mconcat [ mkLabel i | i <- [0 .. n], isBigTick i]

-- | Draw bined diagram from server data and mouse position
binedDiagram :: BinedGraph -> Diagram B
binedDiagram graph = centerXY $ frame 0.2 $
  (alignL $ strutX labelOffset ||| tline')
  === 
  strutY 0.5 
  === 
  bins'
  where
  labelOffset = (/ 4) . fromIntegral . maximum 
    $ T.length . binLineName 
    <$> (V.concat . V.toList . fmap lineGroupLines . binedGraphLines $ graph)
  tline' = mkTimeLine $ extractTimeLine graph
  bins' = vcat $ V.toList $ mkLineGroup labelOffset <$> binedGraphLines graph

-- | Special widget for bined graph dedug
binnedDiagramDebugWidget :: MonadWidget t m => m ()
binnedDiagramDebugWidget = mdo
  -- -- svgDyn :: Dynamic t (m (DiaEv Any))
  -- svgDyn <- mapDyn (reflexDia $ def & sizeSpec .~ dims2D 1920 1000) diaDyn
  -- -- pos :: Event (P2 Double)
  -- pos <- switchPromptly never <$> fmap diaMousemovePos =<< dyn svgDyn
  -- -- diaDyn :: Dynamic (Diagram B)
  -- diaDyn <- holdDyn (binedDiagram graph . p2 $ (0, -1000)) (binedDiagram graph <$> pos)
  let dia = binedDiagram graph
  _ <- reflexDia (def & sizeSpec .~ dims2D 1920 1000) dia
  return ()
  where 
  bins = V.fromList [
      LineGroup "GC" (V.fromList [
          BinLine "GC" (toRGB red) 0 $ VU.fromList $ take 50 $ cycle sample0
        ])
    , LineGroup "Thread Events" (V.fromList [
        BinLine "1" (toRGB blue) 0 $ VU.fromList $ take 50 $ cycle sample1
      , BinLine "2" (toRGB blue) 7 $ VU.fromList $ take 10 $ cycle sample2
      ])
    ]
  sample0 = [0.0, 0.1, 0.1, 0.2, 0.0, 0.5, 0.8, 0.3, 0.1, 0.2, 0.0, 0.1, 0.1]
  sample1 = [0.5, 0.4, 0.3, 0.5, 0.9, 1.0, 1.0, 0.1, 0.4, 0.5, 0.7, 0.9, 0.4]
  sample2 = [0.5, 0.9, 1.0, 1.0, 0.1, 0.4, 0.5]
  lineLengths = fmap (VU.length . binLineValues) . lineGroupLines <$> bins
  maxn = maximum $ V.concat $ V.toList lineLengths
  graph = BinedGraph {
      binedGraphBegin = 0 
    , binedGraphEnd = 10 
    , binedGraphBinWidth = 10 / fromIntegral maxn
    , binedGraphLines = bins
    }