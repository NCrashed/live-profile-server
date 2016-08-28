module Profile.Live.Server.Client.EventLog(
    eventLogWidget
  ) where 

import Control.Monad 
import Control.Monad.Trans.Either
import Data.Aeson.WithField
import Reflex 
import Reflex.Dom 
import Servant.API.Auth.Token 
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive 
import Servant.Client 

import qualified GHC.RTS.Events as E 

import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Bootstrap.Button 
import Profile.Live.Server.Client.Router 

-- | Getting list of events
eventsList :: EventLogId 
  -> Maybe Page 
  -> Maybe PageSize
  -> MToken' '["read-eventlog"]
  -> EitherT ServantError IO (PagedList (Id E.Event) E.Event) 

( eventsList 
  ) = client eventLogAPI Nothing

-- | Widget to display raw eventlog
eventLogWidget :: forall t m . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> Maybe (m (Route t m)) -- ^ Possible widget for "Back" button
  -> EventLogId -- ^ Id of eventlog
  -> m (Route t m)
eventLogWidget tok backW eid = do 
  backE <- blueButton "Back"

  reqE <- fmap (const 0) <$> getPostBuild
  eventsE <- simpleRequest reqE (\p -> eventsList eid (Just p) Nothing (Just (Token tok)))
  _ <- widgetHold emptyWell (renderEvents <$> eventsE)

  return $ maybe (Route never) (\w -> Route $ const w <$> backE) backW
  where 
  emptyWell :: m ()
  emptyWell = elClass "div" "well" $ text "Loading ..."

  renderEvents :: PagedList (Id E.Event) (E.Event) -> m ()
  renderEvents (PagedList es _) = elClass "div" "well" $ do  
    forM_ es $ \(WithField _ e) -> el "p" $ text (show e)
