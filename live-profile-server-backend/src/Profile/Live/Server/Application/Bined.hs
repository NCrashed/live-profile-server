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
import Profile.Live.Server.Application.Bined.Model
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
  runDB $ getFullBinedGraph i (fromMaybe 60 bw) colThreads colGc colCustom

-- | Get full bined graph for event log
getFullBinedGraph :: EventLogId 
  -> Double -- ^ Bin width in seconds
  -> RGB Double -- ^ Color of thread lines
  -> RGB Double -- ^ Color of gc line
  -> RGB Double -- ^ Color of custom line
  -> SqlPersistT IO BinedGraph
getFullBinedGraph i binWidth colThreads colGc colCustom = do 
  binedGraphBegin <- getFirstEventTime
  binedGraphEnd <- getLastEventTime
  let binedGraphBinWidth = binWidth
  binedGraphLines <- getBinLines binedGraphBegin binWidth 
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
  spawnTime <- getThreadSpawnTime logId threadId
  let offset = calcOffset spawnTime

  es <- getThreadEvents logId threadId 
  let (_, _, _, !values) = foldl' collectBins (0, spawnTime, 0, VU.empty) es
  return BinLine {
      binLineName = name 
    , binLineColour = colour 
    , binLineOffset = offset 
    , binLineValues = values 
    }
  where 
  calcOffset = maybe 0 (toBinNumber tOffset binWidth . (subtract tOffset) . fromTimestamp)
  
  collectBins (!curBin, !stopT, !workT, !bins) e = case evSpec e of 
    RunThread{} -> undefined
    StopThread{} -> undefined
    _ -> undefined

-- | Calculate bin number from time
toBinNumber :: Double -- ^ Offset from begining
  -> Double -- ^ Width of bin
  -> Double -- ^ time 
  -> Int 
toBinNumber tOffset binWidth t = floor $ (t - tOffset) / binWidth

-- | Convert timestamp to seconds
fromTimestamp :: Timestamp -> Double 
fromTimestamp = (/ 1000000) . fromIntegral