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
  ) where 

import Database.Persist.Sql 

import Profile.Live.Server.Events.Model

-- | Insert empty eventlog
startEventLog :: SqlPersistT IO EventLogImplId
startEventLog = insert EventLogImpl