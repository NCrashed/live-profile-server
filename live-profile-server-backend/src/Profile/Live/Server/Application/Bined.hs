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
    binedGraphBegin binedGraphEnd colThreads colGc colCustom i
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
  -> Double -- ^ Start time
  -> Double -- ^ End time
  -> RGB Double -- ^ Color of thread lines
  -> RGB Double -- ^ Color of gc line
  -> RGB Double -- ^ Color of custom line
  -> EventLogId -- ^ Id of log
  -> SqlPersistT IO (V.Vector BinLine)
getBinLines tOffset binWidth startT endT colThreads colGc colCustom i = do 
  gcline <- getGcLine tOffset binWidth startT endT colGc i
  threads <- getEventLogThreads i
  tlines <- mapM (getThreadLine tOffset binWidth colThreads i) 
    $ V.fromList threads
  return $ gcline `V.cons` tlines

-- | Calculate workout of GC
getGcLine :: Double -- ^ Time offset from begining 
  -> Double -- ^ Bin width in seconds
  -> Double -- ^ Start time
  -> Double -- ^ End time
  -> RGB Double -- ^ Color of line
  -> EventLogId -- ^ Event log id
  -> SqlPersistT IO BinLine
getGcLine tOffset binWidth starT endT colour logId = do 
  let binsCount = ceiling $ (endT - starT) / binWidth
  values <- VU.generateM binsCount $ calcBin tOffset binWidth getter einterpret
  values `deepseq` return BinLine {
      binLineName = "GC" 
    , binLineColour = colour 
    , binLineOffset = 0 
    , binLineValues = values 
    }
  where 
  getter = getGcEventsInPeriod logId
  einterpret e = case evSpec e of 
    StartGC{} -> WorkoutWork
    EndGC{} -> WorkoutStop
    _ -> WorkoutNone

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
  values <- VU.generateM binsCount $ calcBin tOffset binWidth getter einterpret
    . (offset +)

  values `deepseq` return BinLine {
      binLineName = name 
    , binLineColour = colour 
    , binLineOffset = offset 
    , binLineValues = values 
    }
  where 
  getter = getThreadEventsInPeriod logId threadId
  einterpret e = case evSpec e of 
    StopThread{} -> WorkoutWork
    RunThread{} -> WorkoutStop
    _ -> WorkoutNone

-- | Helper for 'calcBin' to select wether event increments work time
-- or stop time or neither of them.
data WorkoutCase = 
    WorkoutWork
  | WorkoutStop
  | WorkoutNone
  deriving (Eq)

-- | Calculate workout during a bin
calcBin :: Double -- ^ Time offset 
  -> Double -- ^ Bin width in seconds
  -> (Timestamp -> Timestamp -> SqlPersistT IO [Event]) 
  -- ^ Getter of events during period
  -> (Event -> WorkoutCase) -- ^ Interpreter of events
  -> Int -- ^ Bin number
  -> SqlPersistT IO Double
calcBin tOffset binWidth getter einterpret i = do 
  let startT = toTimestamp $ toBinLowBound tOffset binWidth i 
  let endT = toTimestamp $ toBinUpperBound tOffset binWidth i 
  es <- getter startT endT
  let (_, stopT, workT) = foldl' collectTime (startT, 0, 0) es
  let workout = workT / (stopT + workT)
  return $ if isNaN workout then 0 else workout
  where 
  -- | Collect work and stop times
  collectTime :: (Timestamp, Double, Double)
    -> Event 
    -> (Timestamp, Double, Double)
  collectTime (!lastT, !stopT, !workT) e = case einterpret e of 
    WorkoutWork -> (evTime e, stopT + dt, workT)
    WorkoutStop -> (evTime e, stopT, workT + dt)
    WorkoutNone -> (lastT, stopT, workT)
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