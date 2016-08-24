{-|
Module      : Profile.Live.Server.Application.EventLog
Description : Implementation of EventLog API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application.EventLog(
    startEventLog
  , addEventLogType
  , addEventLogEvent
  ) where 

import Control.Monad 
import Database.Persist.Sql 
import GHC.RTS.Events

import Profile.Live.Protocol.State
import Profile.Live.Server.Events.Model

-- | Insert empty eventlog
startEventLog :: SqlPersistT IO EventLogImplId
startEventLog = insert EventLogImpl

-- | Insert event type into eventlog being recording
addEventLogType :: EventLogImplId -> EventType -> SqlPersistT IO EventTypeImplId
addEventLogType lid t = insert $ toEventTypeImpl lid t

-- | Insert event into eventlog being recording
addEventLogEvent :: EventLogImplId -> Event -> SqlPersistT IO EventImplId 
addEventLogEvent lid e = insert $ toEventImpl lid e

-- | Insert state snapshot into eventlog being recording
addEventLogState :: EventLogImplId -> EventlogState -> SqlPersistT IO EventlogStateImplId
addEventLogState lid s = do 
  let (impl, threads, capsets, caps, tasks) = toEventlogStateImpl lid s 
  si <- insert impl 
  mapM_ (void . insert) $ threads si
  forM_ (capsets si) $ \(cs, caps, args, envs) -> do 
    ci <- insert cs 
    mapM_ (void . insert) $ caps ci
    mapM_ (void . insert) $ args ci 
    mapM_ (void . insert) $ envs ci 
  mapM_ (void . insert) $ caps si 
  mapM_ (void . insert) $ tasks si
  return si
