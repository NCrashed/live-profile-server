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
  , addEventLogState
  , eventLogServer
  ) where 

import Control.Monad 
import Data.Aeson.WithField
import Data.Maybe 
import Database.Persist.Sql 
import GHC.RTS.Events
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Server 
import Servant.Server.Auth.Token

import Profile.Live.Protocol.State
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Events.Model
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils

-- | Implementation of eventl log sub API
eventLogServer :: ServerT EventLogAPI App 
eventLogServer = listEvents

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
  forM_ (capsets si) $ \(cs, cps, args, envs) -> do 
    ci <- insert cs 
    mapM_ (void . insert) $ cps ci
    mapM_ (void . insert) $ args ci 
    mapM_ (void . insert) $ envs ci 
  mapM_ (void . insert) $ caps si 
  mapM_ (void . insert) $ tasks si
  return si

-- | Getting event from DB
readEvent :: EventImplId -> SqlPersistT IO (Maybe Event)
readEvent i = do 
  mei <- get i
  return . join $ fromEventImpl <$> mei

-- | Getting events from server with pagination
listEvents :: EventLogId
  -> Maybe Page 
  -> Maybe PageSize
  -> MToken' '["read-eventlog"]
  -> App (PagedList (Id Event) Event)
listEvents ei mp msize token = do 
  guardAuthToken token 
  pagination mp msize $ \page size -> do 
    let filters = [
            EventImplEventLog ==. toKey ei 
          ]
    (es, total) <- runDB $ (,)
      <$> (do
        (is :: [Key EventImpl]) <- selectKeysList filters [OffsetBy (fromIntegral $ page * size), LimitTo (fromIntegral size)]
        forM is $ (\i -> fmap (WithField (Id $ fromKey i)) <$> readEvent i))
      <*> count filters
    return PagedList {
        pagedListItems = catMaybes es
      , pagedListPages = ceiling $ (fromIntegral total :: Double) / fromIntegral size
      }