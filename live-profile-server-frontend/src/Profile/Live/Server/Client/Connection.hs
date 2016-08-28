{-|
Module      : Profile.Live.Server.Client.Connection
Description : Bindings to server API and widgets for connections
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Control.Lens
import Control.Monad.Trans.Either
import Data.Aeson.Unit 
import Data.Aeson.WithField 
import Data.Monoid 
import Data.Proxy
import Data.Time 
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
import Profile.Live.Server.Client.Session
import Profile.Live.Server.Client.Router

type ConnPerm s = MToken '[ 'PermConcat ('PermLabel s) ('PermLabel "connection")]

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
connectionsWidget token = route renderConnections
  where
  renderConnections :: m (Route t m)
  renderConnections = do 
    sessionEvent <- renderList (Just 10) renderConnection requestConns 
    let thisW = renderConnections
    return $ Route $ sessionsWidget token (Just thisW) <$> sessionEvent 

  renderConnection :: WithId (Id Connection) Connection -> m (Event t (Id Connection))
  renderConnection (WithField i conn) = elClass "div" "panel panel-default" $ do 
    elClass "div" "panel-body" $ do
      let name = conn ^. rlens (Proxy :: Proxy '("name", T.Text)) . rfield
          host = conn ^. rlens (Proxy :: Proxy '("host", T.Text)) . rfield
          port = conn ^. rlens (Proxy :: Proxy '("port", Word)) . rfield
          lastUsed = conn ^. rlens (Proxy :: Proxy '("lastUsed", Maybe UTCTime)) . rfield
      elAttr "span" [("style", "font-weight: bold;")] $ text $ T.unpack name
      text $ " (" <> T.unpack host <> ":" <> show port <> ") "
        <> "Last used: " <> show lastUsed
      sessions <- fmap (const i) <$> blueButton "Sessions"
      del <- blueButton "Delete"
      return sessions

  requestConns :: Event t Page -> m (Event t (Page, PagedList (Id Connection) Connection))
  requestConns e = do 
    let mkReq p = (,)
          <$> pure p
          <*> connList (Just p) Nothing (Just (Token token))
    reqEv <- asyncAjax mkReq e
    _ <- widgetHold (pure ()) $ ffor reqEv $ \resp -> case resp of 
      Left er -> danger er 
      Right _ -> return ()    
    let itemsE = either (const $ (0, PagedList [] 0)) id <$> reqEv
    return itemsE 

  danger = elClass "div" "alert alert-danger" . text 