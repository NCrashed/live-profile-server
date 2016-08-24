{-|
Module      : Profile.Live.Server.API.EventLog
Description : Sub API about recorded eventlog
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.API.EventLog(
    EventLogId
  ) where 

-- | Identifier of recorded event log 
type EventLogId = Word 