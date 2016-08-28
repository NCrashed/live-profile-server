module Profile.Live.Server.Client.EventLog(
    eventLogWidget
  ) where 

import Control.Monad.Trans.Either
import GHCJS.Marshal
import Reflex 
import Reflex.Dom 
import Servant.API.Auth.Token 
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive 
import Servant.Client 

import qualified GHC.RTS.Events as E 

import Profile.Live.Server.API.EventLog
import Profile.Live.Server.API.Session
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
eventLogWidget :: MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> Maybe (m (Route t m)) -- ^ Possible widget for "Back" button
  -> Id Session -- ^ Session the log belongs to
  -> m (Route t m)
eventLogWidget tok backW sid = do 
  backE <- blueButton "Back"
  return $ maybe (Route never) (\w -> Route $ const w <$> backE) backW
