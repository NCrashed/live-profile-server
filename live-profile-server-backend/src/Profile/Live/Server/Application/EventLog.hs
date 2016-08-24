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

import Database.Persist.Sql 
import GHC.RTS.Events

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