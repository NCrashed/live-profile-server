{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Profile.Live.Server.Client.EventLog(
  -- * Server API
    eventsList
  , downloadEventLog
  , deleteEventLog
  , importingList
  , importingCancel
  -- * Widget
  , eventLogWidget
  ) where 

import Control.Monad 
import Control.Monad.Trans.Either
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Text (Text)
import GHCJS.Marshal
import Reflex 
import Reflex.Dom 
import Servant.API as S
import Servant.API.Auth.Token 
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive 
import Servant.Client 
import Servant.Common.Req

import qualified GHC.RTS.Events as E

import Profile.Live.Server.API.EventLog
import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Bootstrap.Button 
import Profile.Live.Server.Client.Router 
import Profile.Live.Server.Client.Pagination 
import Profile.Live.Server.Client.Utils 

-- | Getting list of events
eventsList :: EventLogId 
  -> Maybe Page 
  -> Maybe PageSize
  -> MToken' '["read-eventlog"]
  -> EitherT ServantError IO (PagedList (Id E.Event) E.Event) 

-- | Getting list of logs being importing
importingList :: MToken' '["read-eventlog"]
  -> EitherT ServantError IO [EventLogImport]

-- | Canceling eventlog import
importingCancel :: EventLogId
  -> MToken' '["write-eventlog"]
  -> EitherT ServantError IO Unit

downloadEventLog :: EventLogId
--  -> MToken' '["read-eventlog"]
  -> EitherT ServantError IO (
      Headers '[S.Header "Content-Disposition" Text] 
        EventLogFile)

-- | Deleting eventlog from server
deleteEventLog :: EventLogId 
  -> MToken' '["delete-eventlog"]
  -> EitherT ServantError IO Unit

(      eventsList 
  :<|> downloadEventLog
  :<|> importingList
  :<|> importingCancel
  :<|> deleteEventLog
  ) = client eventLogAPI Nothing

instance GHCJSUnrender OctetStream EventLogFile where 
  --ghcjsUnrender :: Proxy ctype -> JSVal -> IO (Either String a)
  ghcjsUnrender _ _ = return $ Left "Unsupported"

instance FromJSVal EventLogImport where 
  fromJSVal = fromJSVal_aeson

-- | Widget to display raw eventlog
eventLogWidget :: forall t m . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> Maybe (m (Route t m)) -- ^ Possible widget for "Back" button
  -> EventLogId -- ^ Id of eventlog
  -> m (Route t m)
eventLogWidget tok backW eid = do 
  header "Event Log"
  backE <- centered $ buttonGroup $ do 
    backE <- blueButton "Back"
    downloadButton "Download"
    return backE

  _ <- renderPage (Just 10) renderEvents requestEvents

  return $ maybe (Route never) (\w -> Route $ const w <$> backE) backW
  where 

  renderEvents :: Page -> PagedList (Id E.Event) (E.Event) -> m (Event t a)
  renderEvents _ (PagedList es _) = elClass "div" "well" $ do  
    forM_ es $ \(WithField _ e) -> el "p" $ text (show e)
    return never

  requestEvents :: Event t Page -> m (Event t (Page, PagedList (Id E.Event) E.Event))
  requestEvents ep = simpleRequest ep (\p -> (,)
    <$> pure p
    <*> eventsList eid (Just p) Nothing (Just (Token tok)))

  downloadButton :: String -> m ()
  downloadButton lbl = elAttr "a" [
      ("class", "btn btn-primary")
    , ("href", "/eventlog/download/" ++ show eid)
    ] $ text lbl