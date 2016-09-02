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
import Data.Ord 
import Database.Persist.Sql
import GHC.RTS.Events 
import Servant.API.Auth.Token
import Servant.Server 
import Servant.Server.Auth.Token

import qualified Data.Filterable as F 
import qualified Data.HashMap.Strict as H 
import qualified Data.List as L 
import qualified Data.Sequence as S 
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
  let defaultWidth' = (binedGraphEnd - binedGraphBegin) / fromIntegral (50 :: Int)
      defaultWidth = if defaultWidth' == 0 then 1 else defaultWidth'
  let binedGraphBinWidth' = fromMaybe defaultWidth mbinWidth
      binedGraphBinWidth = if binedGraphBinWidth' == 0 then 1 else binedGraphBinWidth'
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
  -> SqlPersistT IO (V.Vector LineGroup)
getBinLines tOffset binWidth startT endT colThreads colGc colCustom i = do 
  gcline <- getGcLine tOffset binWidth startT endT colGc i
  clines <- getCustomLines tOffset binWidth colCustom i
  threads <- getEventLogThreads i
  tlines <- mapM (getThreadLine tOffset binWidth colThreads i) 
    $ V.fromList threads
  return $ V.fromList [
      LineGroup "GC Events" (V.singleton gcline)
    , LineGroup "User Events" clines
    , LineGroup "Thread Events" tlines
    ]

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
  values <- calcBinsM tOffset binWidth binsCount 0 getter einterpret
  values `deepseq` return BinLine {
      binLineName = "GC" 
    , binLineColour = colour 
    , binLineOffset = 0 
    , binLineValues = values 
    }
  where 
  getter = getGcEventsInPeriod logId
  einterpret e = case evSpec e of 
    StartGC{} -> Just . WorkoutWork . evTime $ e
    EndGC{} -> Just . WorkoutStop . evTime $ e
    _ -> Nothing

-- | Get bined lines for custom events
getCustomLines :: Double -- ^ Time offset from begining 
  -> Double -- ^ Bin width in seconds
  -> RGB Double -- ^ Color of line
  -> EventLogId -- ^ Event log id 
  -> SqlPersistT IO (V.Vector BinLine)
getCustomLines tOffset binWidth colour logId = do 
  es <- splitCustomEvents <$> getUserEvents logId
  return $ V.fromList . toList . sortByTime $ H.foldlWithKey' go mempty es
  where 
  go :: S.Seq (Timestamp, BinLine) -> String -> S.Seq UserEvent -> S.Seq (Timestamp, BinLine)
  go !bls !name !es = bls S.|> (appearTime, line)
    where 
    line = getCustomLine tOffset binWidth colour name es
    appearTime = case S.viewl es of 
      S.EmptyL -> 0
      e S.:< _ -> userEvTime e 

  sortByTime :: S.Seq (Timestamp, BinLine) -> S.Seq BinLine
  sortByTime = fmap snd . S.sortBy (comparing fst)

-- | Helper to carry info about user event start and end
data UserEvent = 
    UserEventStart !Timestamp
  | UserEventEnd !Timestamp
  deriving (Eq, Show)

-- | Extract event time
userEvTime :: UserEvent -> Timestamp
userEvTime ue = case ue of 
  UserEventStart t -> t 
  UserEventEnd t -> t 

-- | Split custom events by names
splitCustomEvents :: [Event] -> H.HashMap String (S.Seq UserEvent)
splitCustomEvents = foldl' go mempty
  where 
  go :: H.HashMap String (S.Seq UserEvent)
    -> Event 
    -> H.HashMap String (S.Seq UserEvent)
  go !m !e = case evSpec e of 
    UserMessage{..} -> case L.stripPrefix "START " msg of
      Just name -> addEvent name UserEventStart
      Nothing -> case L.stripPrefix "END " msg of
        Just name -> addEvent name UserEventEnd
        Nothing -> m
    _ -> m
    where 
    addEvent name mkEventType = case H.lookup name m of 
      Nothing -> H.insert name (S.singleton ue) m
      Just es -> H.insert name (es S.|> ue) m
      where 
        ue = mkEventType $ evTime e

-- | Get bin line about custom line event from eventlog
getCustomLine :: Double -- ^ Time offset from begining 
  -> Double -- ^ Bin width in seconds
  -> RGB Double -- ^ Color of line
  -> String -- ^ Custom event name 
  -> S.Seq UserEvent -- ^ Events related to event
  -> BinLine
getCustomLine tOffset binWidth colour name es = 
  values `deepseq` BinLine {
      binLineName = T.pack name 
    , binLineColour = colour 
    , binLineOffset = offset 
    , binLineValues = values 
    }
  where 
  appearTime = case S.viewl es of 
    S.EmptyL -> 0
    e S.:< _ -> fromTimestamp $ userEvTime e 
  lastTime = case S.viewr es of 
    S.EmptyR -> 0 
    _ S.:> e -> fromTimestamp $ userEvTime e 
  offset = toBinNumber tOffset binWidth appearTime 
  binsCount = ceiling $ (lastTime - appearTime) / binWidth
  values = calcBins tOffset binWidth binsCount offset es einterpret
  einterpret e = Just $ case e of 
    UserEventStart t -> WorkoutWork t
    UserEventEnd t -> WorkoutStop t

-- | Get bin line about particular thread from eventlog 
getThreadLine :: Double -- ^ Time offset from begining 
  -> Double -- ^ Bin width in seconds
  -> RGB Double -- ^ Color of line
  -> EventLogId -- ^ Event log id
  -> ThreadId -- ^ Thread id we collects info about
  -> SqlPersistT IO BinLine
getThreadLine tOffset binWidth colour logId threadId = do
  mlabel <- fmap (++ " (" ++ show threadId ++ ")") <$> getThreadLabel logId threadId 
  let name = maybe (showt threadId) T.pack mlabel
  spawnTime <- maybe 0 fromTimestamp <$> getThreadSpawnTime logId threadId
  dieTime <- maybe 0 fromTimestamp <$> getThreadLastTime logId threadId 
  
  let offset = toBinNumber tOffset binWidth  spawnTime 
  let binsCount = ceiling $ (dieTime - spawnTime) / binWidth
  values <- calcBinsM tOffset binWidth binsCount offset getter einterpret

  values `deepseq` return BinLine {
      binLineName = name 
    , binLineColour = colour 
    , binLineOffset = offset 
    , binLineValues = values 
    }
  where 
  getter = getThreadEventsInPeriod logId threadId
  einterpret e = case evSpec e of 
    StopThread{} -> Just . WorkoutWork . evTime $ e
    RunThread{} -> Just . WorkoutStop . evTime $ e
    _ -> Nothing

-- | Helper for 'calcBin' to select whether event increments work time
-- or stop time or neither of them.
data WorkoutCase = 
    WorkoutWork Timestamp
  | WorkoutStop Timestamp
  deriving (Eq)

-- | Helper to extract timestamp from case
workoutCaseTimestamp :: WorkoutCase -> Timestamp
workoutCaseTimestamp wc = case wc of 
  WorkoutWork t -> t 
  WorkoutStop t -> t 

-- | Helper that calculates all bins in once
calcBinsM :: Foldable f => Double -- ^ Time offset 
  -> Double -- ^ Bin width in seconds
  -> Int -- ^ Bins count
  -> Int -- ^ Bins offset
  -> (Timestamp -> Timestamp -> SqlPersistT IO (f a)) 
  -- ^ Getter of events during period
  -> (a -> Maybe WorkoutCase) -- ^ Interpreter of events
  -> SqlPersistT IO (VU.Vector Double) 
calcBinsM tOffset binWidth binsCount offset getter einterpret = do
  (_ , values) <- foldlM go (False, mempty) $ (+offset) <$> [0 .. binsCount-1]
  return $ VU.fromList $ toList values
  where 
  go :: (Bool, S.Seq Double) -> Int -> SqlPersistT IO (Bool, S.Seq Double)
  go (!begRun, !bins) i = do 
    (endRun, !b) <- calcBinM tOffset binWidth getter einterpret begRun i
    return (endRun, bins S.|> b)

-- | Helper that calculates all bins in once
calcBins :: forall f a . (Foldable f, F.Filterable f a, F.FilterConstr f a) 
  => Double -- ^ Time offset 
  -> Double -- ^ Bin width in seconds
  -> Int -- ^ Bins count
  -> Int -- ^ Bins offset
  -> f a
  -- ^ Getter of events during period
  -> (a -> Maybe WorkoutCase) -- ^ Interpreter of events
  -> VU.Vector Double
calcBins tOffset binWidth binsCount offset es einterpret = VU.fromList $ toList values
  where 
  (_ , values) = foldl' go (False, mempty) $ (+offset) <$> [0 .. binsCount-1]
  go :: (Bool, S.Seq Double) -> Int -> (Bool, S.Seq Double)
  go (!begRun, !bins) i = let
    es' = F.filter (eventsInBin i) es
    (endRun, !b) = calcBin tOffset binWidth es' einterpret begRun i
    in (endRun, bins S.|> b)

  eventsInBin :: Int -> a -> Bool 
  eventsInBin i a = let 
    startT = toTimestamp $ toBinLowBound tOffset binWidth i 
    endT = toTimestamp $ toBinUpperBound tOffset binWidth i
    t = maybe 0 workoutCaseTimestamp $ einterpret a
    in t >= startT && t < endT


-- | Calculate workout during a bin
calcBinM :: Foldable f => Double -- ^ Time offset 
  -> Double -- ^ Bin width in seconds
  -> (Timestamp -> Timestamp -> SqlPersistT IO (f a)) 
  -- ^ Getter of events during period
  -> (a -> Maybe WorkoutCase) -- ^ Interpreter of events
  -> Bool -- ^ Does the thread/event is running at begining of bin
  -> Int -- ^ Bin number
  -> SqlPersistT IO (Bool, Double) 
  -- ^ Is the thread is not finished work at the end of bin 
  -- and workout value from 0 to 1
calcBinM tOffset binWidth getter einterpret begRun i = do 
  let startT = toTimestamp $ toBinLowBound tOffset binWidth i 
  let endT = toTimestamp $ toBinUpperBound tOffset binWidth i 
  es <- getter startT endT 
  return $ calcBin tOffset binWidth es einterpret begRun i

-- | Calculate workout during a bin
calcBin :: forall f a . Foldable f
  => Double -- ^ Time offset 
  -> Double -- ^ Bin width in seconds
  -> f a -- ^ Sequence of events
  -- ^ Getter of events during period
  -> (a -> Maybe WorkoutCase) -- ^ Interpreter of events
  -> Bool -- ^ Does the thread/event is running at begining of bin
  -> Int -- ^ Bin number
  -> (Bool, Double) 
  -- ^ Is the thread is not finished work at the end of bin 
  -- and workout value from 0 to 1
calcBin tOffset binWidth es einterpret begRun i = (endRun, workout)
  where 
  startT = toTimestamp $ toBinLowBound tOffset binWidth i 
  endT = toTimestamp $ toBinUpperBound tOffset binWidth i 

  (lastT, endRun, stopT', workT') = foldl' collectTime (startT, begRun, 0, 0) es
  endingT = fromTimestamp $ endT - lastT
  stopT = stopT' + if endRun then 0 else endingT
  workT = workT' + if endRun then endingT else 0 

  workout' = workT / (stopT + workT)
  workout = if isNaN workout' then 0 else workout'

  -- | Collect work and stop times
  collectTime :: (Timestamp, Bool, Double, Double)
    -> a 
    -> (Timestamp, Bool, Double, Double)
  collectTime (!lt, !br, !st, !wt) e = case einterpret e of 
    Just (WorkoutWork t) -> (t, True, st + dt t, wt)
    Just (WorkoutStop t) -> (t, False, st, wt + dt t)
    Nothing -> (lt, br, st, wt)
    where 
    dt t = fromTimestamp (t - lt)

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