{-# LANGUAGE OverloadedLists #-}
module Profile.Live.Server.Client.Connection(
  -- * Server API
    connGet
  , connPost
  , connPut
  , connPatch
  , connDelete
  , connList
  -- * Widgets
  , connectionsWidget
  ) where 

import Control.Monad.Trans.Either
import Data.Aeson.Unit 
import Data.Aeson.WithField 
import Data.Monoid 
import Data.Vinyl
import GHCJS.Marshal
import Reflex as R
import Reflex.Dom as R
import Servant.API
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Client 

import qualified Data.Text as T 

import Profile.Live.Server.API.Connection
import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Bootstrap.Button
import Profile.Live.Server.Client.Pagination

type ConnPerm s = MToken '[PermConcat (PermLabel s) (PermLabel "connection")]

connGet :: Id Connection
  -> ConnPerm "read-"
  -> EitherT ServantError IO Connection

connPost :: Connection
  -> ConnPerm "create-"
  -> EitherT ServantError IO (OnlyId (Id Connection))

connPut :: Id Connection
  -> Connection
  -> ConnPerm "write-"
  -> EitherT ServantError IO Unit

connPatch :: Id Connection
  -> PatchRec Connection
  -> ConnPerm "write-"
  -> EitherT ServantError IO Unit

connDelete :: Id Connection
  -> ConnPerm "delete-"
  -> EitherT ServantError IO Unit

connList :: Maybe Page 
  -> Maybe PageSize 
  -> MToken' '["read-connection"]
  -> EitherT ServantError IO (PagedList (Id Connection) Connection)

(      (connGet
  :<|> connPost
  :<|> connPut
  :<|> connPatch
  :<|> connDelete)
  :<|> connList
    ) = client connectionAPI Nothing

instance ToJSVal ConnectionPatch where 
  toJSVal = toJSVal_aeson

-- | Render list of connections with pagination
connectionsWidget :: forall t m . MonadWidget t m => SimpleToken -> m ()
connectionsWidget token = do 
  reqE <- fmap (const 0) <$> getPostBuild
  dataE <- requestConns reqE
  let widgetE = renderList renderConnection <$> dataE
  widgetHold (pure ()) widgetE
  return ()
  where 

  renderConnection :: WithId (Id Connection) Connection -> m ()
  renderConnection (WithField _ (
       Field name 
    :& Field host 
    :& Field port 
    :& Field lastUsed
    :& RNil )) = elClass "div" "panel panel-default" $ do 
      elClass "div" "panel-body" $ do
        elAttr "span" [("style", "font-weight: bold;")] $ text $ T.unpack name
        text $ " (" <> T.unpack host <> ":" <> show port <> ") "
          <> "Last used: " <> show lastUsed
        sessions <- blueButton "Sessions"
        del <- blueButton "Delete"
        return ()

  requestConns :: Event t Page -> m (Event t (PagedList (Id Connection) Connection))
  requestConns e = do 
    let mkReq p = connList (Just p) Nothing (Just (Token token))
    reqEv <- asyncAjax mkReq e
    widgetHold (pure ()) $ ffor reqEv $ \resp -> case resp of 
      Left er -> danger er 
      Right _ -> return ()    
    let itemsE = either (const $ PagedList [] 0) id <$> reqEv
    return itemsE 

  danger = elClass "div" "alert alert-danger" . text 