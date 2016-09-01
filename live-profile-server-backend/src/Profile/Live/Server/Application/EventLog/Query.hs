{-|
Module      : Profile.Live.Server.Application.EventLog.Query
Description : Complex queries for event log
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application.EventLog.Query(
    getEventLogThreads
  , readEventLogEvents
  , getEventLogFirstEvent
  , getEventLogLastEvent
  , getThreadLabel
  , getThreadSpawnTime
  , getThreadLastTime
  , getThreadEvents
  , getThreadEventsInPeriod
  ) where 

import Control.Monad 
import Data.Maybe hiding (isNothing)
import Database.Esqueleto
import GHC.RTS.Events hiding (desc)
import Safe 

import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Events.Model
import Profile.Live.Server.Utils 

-- | Get list of all threads that are presented in the log
getEventLogThreads :: EventLogId -> SqlPersistT IO [ThreadId]
getEventLogThreads i = fmap (catMaybes . fmap unValue) $ do 
  select $ from $ \(e `InnerJoin` ei) -> do
    on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
    orderBy [asc $ e ^. EventImplTime]
    where_ (
          e ^. EventImplEventLog ==. val (toKey i)
      &&. ei ^. EventInfoImplType ==. val createThreadEventType
      &&. not_ (isNothing $ ei ^. EventInfoImplThread))
    return $ ei ^. EventInfoImplThread

-- | Getting events from DB
readEventLogEvents :: EventLogImplId -> SqlPersistT IO [Event]
readEventLogEvents i = do 
  (es :: [(Entity EventImpl, Entity EventInfoImpl)]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      orderBy [asc $ e ^. EventImplTime]
      where_ (e ^. EventImplEventLog ==. val i)
      return (e, ei)
  return $ catMaybes $ uncurry fromEventEntityImpl <$> es

-- | Getting first occured event from eventlog
getEventLogFirstEvent :: EventLogId -> SqlPersistT IO (Maybe Event)
getEventLogFirstEvent i = do 
  (es :: [(Entity EventImpl, Entity EventInfoImpl)]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      orderBy [asc $ e ^. EventImplTime]
      where_ (e ^. EventImplEventLog ==. val (toKey i))
      limit 1
      return (e, ei)
  return $ join . headMay $ uncurry fromEventEntityImpl <$> es

-- | Getting last occured event from eventlog
getEventLogLastEvent :: EventLogId -> SqlPersistT IO (Maybe Event)
getEventLogLastEvent i = do 
  (es :: [(Entity EventImpl, Entity EventInfoImpl)]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      orderBy [desc $ e ^. EventImplTime]
      where_ (e ^. EventImplEventLog ==. val (toKey i))
      limit 1
      return (e :: SqlExpr (Entity EventImpl), ei :: SqlExpr (Entity EventInfoImpl))
  return $ join . headMay $ uncurry fromEventEntityImpl <$> es

-- | Getting label of thread from eventlog
getThreadLabel :: EventLogId -> ThreadId -> SqlPersistT IO (Maybe String)
getThreadLabel i tid = do
  (vs :: [Value (Maybe String)]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      where_ (
            e ^. EventImplEventLog ==. val (toKey i)
        &&. ei ^. EventInfoImplThread ==. val (Just tid)
        &&. not_ (isNothing $ ei ^. EventInfoImplThreadlabel))
      limit 1
      return (ei ^. EventInfoImplThreadlabel)
  return $ join . headMay . fmap unValue $ vs

-- | Get timestamp when the thread was spawned
getThreadSpawnTime :: EventLogId -> ThreadId -> SqlPersistT IO (Maybe Timestamp)
getThreadSpawnTime i tid = do
  (vs :: [Value Timestamp]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      where_ (
            e ^. EventImplEventLog ==. val (toKey i)
        &&. ei ^. EventInfoImplThread ==. val (Just tid)
        &&. ei ^. EventInfoImplType ==. val createThreadEventType)
      limit 1
      return (e ^. EventImplTime)
  return $ headMay . fmap unValue $ vs

-- | Get timestamp of last event for the thread
getThreadLastTime :: EventLogId -> ThreadId -> SqlPersistT IO (Maybe Timestamp)
getThreadLastTime i tid = do
  (vs :: [Value Timestamp]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      orderBy [desc $ e ^. EventImplTime]
      where_ (
            e ^. EventImplEventLog ==. val (toKey i)
        &&. ei ^. EventInfoImplThread ==. val (Just tid))
      limit 1
      return (e ^. EventImplTime)
  return $ headMay . fmap unValue $ vs

-- | Get events related only to the particular thread
getThreadEvents :: EventLogId -> ThreadId -> SqlPersistT IO [Event]
getThreadEvents i tid = do
  (vs :: [(Entity EventImpl, Entity EventInfoImpl)]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      orderBy [asc $ e ^. EventImplTime]
      where_ (
            e ^. EventImplEventLog ==. val (toKey i)
        &&. ei ^. EventInfoImplThread ==. val (Just tid))
      return (e, ei)
  return $ catMaybes $ uncurry fromEventEntityImpl <$> vs

-- | Get events related only to the particular thread in given time interval
getThreadEventsInPeriod :: EventLogId -> ThreadId 
  -> Timestamp -- ^ Begin timestamp [including]
  -> Timestamp -- ^ End timestamp [not including]
  -> SqlPersistT IO [Event]
getThreadEventsInPeriod i tid startT endT = do
  (vs :: [(Entity EventImpl, Entity EventInfoImpl)]) <- select $ 
    from $ \(e `InnerJoin` ei) -> do
      on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
      orderBy [asc $ e ^. EventImplTime]
      where_ (
            e ^. EventImplEventLog ==. val (toKey i)
        &&. ei ^. EventInfoImplThread ==. val (Just tid)
        &&. e ^. EventImplTime >=. val startT
        &&. e ^. EventImplTime <. val endT)
      return (e, ei)
  return $ catMaybes $ uncurry fromEventEntityImpl <$> vs