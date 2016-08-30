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
  , getThreadEvents
  ) where 

import Control.Monad 
import Data.Maybe 
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
      &&. ei ^. EventInfoImplThread !=. val Nothing)
    return $ ei ^. EventInfoImplThread

-- | Getting events from DB
readEventLogEvents :: EventLogImplId -> SqlPersistT IO [Event]
readEventLogEvents i = do 
  es <- select $ from $ \(e `InnerJoin` ei) -> do
    on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
    orderBy [asc $ e ^. EventImplTime]
    where_ (e ^. EventImplEventLog ==. val i)
    return (e, ei)
  return $ catMaybes $ (\(Entity _ e, Entity _ ei) -> fromEventImpl e ei) <$> es

-- | Getting first occured event from eventlog
getEventLogFirstEvent :: EventLogId -> SqlPersistT IO (Maybe Event)
getEventLogFirstEvent i = do 
  es <- select $ from $ \(e `InnerJoin` ei) -> do
    on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
    orderBy [asc $ e ^. EventImplTime]
    where_ (e ^. EventImplEventLog ==. val (toKey i))
    limit 1
    return (e, ei)
  return $ join . headMay $ 
    (\(Entity _ e, Entity _ ei) -> fromEventImpl e ei) <$> es

-- | Getting last occured event from eventlog
getEventLogLastEvent :: EventLogId -> SqlPersistT IO (Maybe Event)
getEventLogLastEvent i = do 
  es <- select $ from $ \(e `InnerJoin` ei) -> do
    on (e ^. EventImplSpec ==. ei ^. EventInfoImplId)
    orderBy [desc $ e ^. EventImplTime]
    where_ (e ^. EventImplEventLog ==. val (toKey i))
    limit 1
    return (e, ei)
  return $ join . headMay $ 
    (\(Entity _ e, Entity _ ei) -> fromEventImpl e ei) <$> es

-- | Getting label of thread from eventlog
getThreadLabel :: EventLogId -> ThreadId -> SqlPersistT IO (Maybe String)
getThreadLabel = undefined

-- | Get timestamp when the thread was spawned
getThreadSpawnTime :: EventLogId -> ThreadId -> SqlPersistT IO (Maybe Timestamp)
getThreadSpawnTime = undefined

-- | Get events related only to the particular thread
getThreadEvents :: EventLogId -> ThreadId -> SqlPersistT IO [Event]
getThreadEvents = undefined