{-|
Module      : Profile.Live.Server.Application.Bined
Description : Implementation of Bined graph API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE BangPatterns #-}
module Profile.Live.Server.Application.Bined(
    binedServer
  ) where 

import Control.DeepSeq 
import Data.Colour.Names
import Data.Colour.SRGB.Linear
import Data.Foldable 
import Data.Maybe 
import Database.Persist.Sql
import GHC.RTS.Events 
import Servant.API.Auth.Token
import Servant.Server 
import Servant.Server.Auth.Token

import qualified Data.Text as T 
import qualified Data.Vector as V 
import qualified Data.Vector.Unboxed as VU 

import Profile.Live.Server.API.Bined 
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Application.EventLog
import Profile.Live.Server.Application.EventLog.Query
--import Profile.Live.Server.Application.Bined.Model
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils

-- | Implementation of server for Bined graph API
binedServer :: ServerT BinedAPI App 
binedServer = fullBinedGraph

-- | Get full bined graph for event log
fullBinedGraph :: EventLogId 
  -> Maybe Double -- ^ Bin width in seconds
  -> Maybe (RGB Double) -- ^ Colour of thread lines
  -> Maybe (RGB Double) -- ^ Colour of gc line
  -> Maybe (RGB Double) -- ^ Colour of custom events
  -> MToken' '["bined-graph"] -- ^ Authorisation token
  -> App BinedGraph 
fullBinedGraph i bw mcolThreads mcolGc mcolCustom token = do 
  guardAuthToken token 
  let colThreads = fromMaybe (toRGB blue) mcolThreads
  let colGc = fromMaybe (toRGB red) mcolGc
  let colCustom = fromMaybe (toRGB green) mcolCustom
  runDB $ getFullBinedGraph i bw colThreads colGc colCustom

-- | Get full bined graph for event log
getFullBinedGraph :: EventLogId 
  -> Maybe Double -- ^ Bin width in seconds, if Nothing it is calculated automatically
  -> RGB Double -- ^ Color of thread lines
  -> RGB Double -- ^ Color of gc line
  -> RGB Double -- ^ Color of custom line
  -> SqlPersistT IO BinedGraph
getFullBinedGraph i mbinWidth colThreads colGc colCustom = do 
  binedGraphBegin <- getFirstEventTime
  binedGraphEnd <- getLastEventTime
  let defaultWidth = (binedGraphEnd - binedGraphBegin) / fromIntegral (50 :: Int)
  let binedGraphBinWidth = fromMaybe defaultWidth mbinWidth
  binedGraphLines <- getBinLines binedGraphBegin binedGraphBinWidth 
    colThreads colGc colCustom i
  return BinedGraph{..}
  where 
  getFirstEventTime = 
    maybe 0 (fromTimestamp . evTime) <$> getEventLogFirstEvent i
  getLastEventTime =
    maybe 0 (fromTimestamp . evTime) <$> getEventLogLastEvent i

-- | Get attached bin lines of event log
--
-- Each bin line is associated with thread or user event.
getBinLines :: Double -- ^ Time offset from begining 
  -> Double -- ^ Bin width in seconds
  -> RGB Double -- ^ Color of thread lines
  -> RGB Double -- ^ Color of gc line
  -> RGB Double -- ^ Color of custom line
  -> EventLogId -- ^ Id of log
  -> SqlPersistT IO (V.Vector BinLine)
getBinLines tOffset binWidth colThreads colGc colCustom i = do 
  threads <- getEventLogThreads i
  mapM (getThreadLine tOffset binWidth colThreads i) $ V.fromList threads

-- | Get bin line about particular thread from eventlog 
getThreadLine :: Double -- ^ Time offset from begining 
  -> Double -- ^ Bin width in seconds
  -> RGB Double -- ^ Color of line
  -> EventLogId -- ^ Event log id
  -> ThreadId -- ^ Thread id we collects info about
  -> SqlPersistT IO BinLine
getThreadLine tOffset binWidth colour logId threadId = do 
  name <- maybe (showt threadId) T.pack <$> getThreadLabel logId threadId
  spawnTime <- maybe 0 fromTimestamp <$> getThreadSpawnTime logId threadId
  dieTime <- maybe 0 fromTimestamp <$> getThreadLastTime logId threadId 
  
  let offset = toBinNumber tOffset binWidth  spawnTime 
  let binsCount = ceiling $ (dieTime - spawnTime) / binWidth
  values <- VU.generateM binsCount $ calcBin . (offset +)

  values `deepseq` return BinLine {
      binLineName = name 
    , binLineColour = colour 
    , binLineOffset = offset 
    , binLineValues = values 
    }
  where   
  -- | Calculate workout in time bin
  calcBin :: Int -> SqlPersistT IO Double
  calcBin i = do 
    let startT = toTimestamp $ toBinLowBound tOffset binWidth i 
    let endT = toTimestamp $ toBinUpperBound tOffset binWidth i 
    es <- getThreadEventsInPeriod logId threadId startT endT
    let (_, stopT, workT) = foldl' collectTime (startT, 0, 0) es
    let workout = workT / (stopT + workT)
    return $ if isNaN workout then 0 else workout

  -- | Collect work and stop times
  collectTime :: (Timestamp, Double, Double)
    -> Event 
    -> (Timestamp, Double, Double)
  collectTime (!lastT, !stopT, !workT) e = case evSpec e of 
    StopThread{} -> (evTime e, stopT + dt, workT)
    RunThread{} -> (evTime e, stopT, workT + dt)
    _ -> (lastT, stopT, workT)
    where 
    dt = fromTimestamp (evTime e - lastT)

-- | Calculate bin number from time
toBinNumber :: Double -- ^ Offset from begining
  -> Double -- ^ Width of bin
  -> Double -- ^ time in seconds
  -> Int 
toBinNumber tOffset binWidth t = floor $ (t - tOffset) / binWidth

-- | Calculate upper time bound of bin
toBinLowBound :: Double -- ^ Offset from begining
  -> Double -- ^ Width of bin 
  -> Int -- ^ Bin number
  -> Double -- ^ Seconds
toBinLowBound tOffset binWidth i = fromIntegral i * binWidth + tOffset

-- | Calculate upper time bound of bin
toBinUpperBound :: Double -- ^ Offset from begining
  -> Double -- ^ Width of bin 
  -> Int -- ^ Bin number
  -> Double -- ^ Seconds
toBinUpperBound tOffset binWidth i = toBinLowBound tOffset binWidth (i+1)

-- | Convert timestamp to seconds
fromTimestamp :: Timestamp -> Double 
fromTimestamp = (/ 1000000000) . fromIntegral

-- | Convert seconds into timestamp
toTimestamp :: Double -> Timestamp 
toTimestamp = round . (* 1000000000)