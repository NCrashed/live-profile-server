{-# LANGUAGE RecursiveDo #-}
module Profile.Live.Server.Client.Router(
    Route(..)
  , route
  ) where 

import Reflex
import Reflex.Dom 

-- | Like 'Fix' for FRP widget, allow to endlessly jump into
-- returned routes of widgets
newtype Route t m = Route { unRoute :: Event t (m (Route t m)) }

-- | Run widget that can replace itself with new widget constructed
-- internally in the original widget.
route :: forall t m . MonadWidget t m => m (Route t m) -> m ()
route w = do 
  rec (rd :: Dynamic t (Route t m)) <- widgetHold w re
      (rd' :: Dynamic t (Event t (m (Route t m)))) <- mapDyn unRoute rd
      let (re :: Event t (m (Route t m))) = switchPromptlyDyn rd'
  return ()