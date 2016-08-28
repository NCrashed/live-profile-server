module Profile.Live.Server.Client.EventLog(
    eventLogWidget
  ) where 

import Reflex 
import Reflex.Dom 

import Servant.API.Auth.Token 
import Servant.API.REST.Derive 

import Profile.Live.Server.API.Session
import Profile.Live.Server.Client.Router 
import Profile.Live.Server.Client.Bootstrap.Button 

-- | Widget to display raw eventlog
eventLogWidget :: MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> Maybe (m (Route t m)) -- ^ Possible widget for "Back" button
  -> Id Session -- ^ Session the log belongs to
  -> m (Route t m)
eventLogWidget tok backW sid = do 
  backE <- blueButton "Back"
  return $ maybe (Route never) (\w -> Route $ const w <$> backE) backW