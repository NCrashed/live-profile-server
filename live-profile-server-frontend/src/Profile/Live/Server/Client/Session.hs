{-|
Module      : Profile.Live.Server.Client.Session
Description : Bindings to server API and widgets for sessions
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
module Profile.Live.Server.Client.Session(
  -- * Server API
    sessionGet
  , sessionList
  , sessionConnect
  , sessionDisconnect
  -- * Widgets
  , sessionsWidget
  ) where 

import Control.Lens
import Control.Monad.Trans.Either
import Data.Aeson.Unit 
import Data.Aeson.WithField 
import Data.Monoid 
import Data.Proxy 
import Data.Time
import Data.Vinyl
import Reflex as R
import Reflex.Dom as R
import Servant.API
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Client 

import Profile.Live.Server.API.Connection
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.API.Session
import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Bootstrap.Button
import Profile.Live.Server.Client.EventLog
import Profile.Live.Server.Client.Pagination
import Profile.Live.Server.Client.Router

type SessPerm s = MToken '[ 'PermConcat ( 'PermLabel s) ( 'PermLabel "session")]

sessionGet :: Id Session
  -> SessPerm "read-"
  -> EitherT ServantError IO Session

sessionList :: Maybe Page 
  -> Maybe PageSize 
  -> Maybe (Id Connection)
  -> MToken' '["read-session"]
  -> EitherT ServantError IO (PagedList (Id Session) Session)

sessionConnect :: Id Connection
  -- -> MToken' '["connect-session"]
  -> EitherT ServantError IO (Id Session)

sessionDisconnect :: Id Session 
  -- -> MToken' '["connect-session"]
  -> EitherT ServantError IO Unit

(      sessionGet
  :<|> sessionList
  :<|> sessionConnect
  :<|> sessionDisconnect
    ) = client sessionAPI Nothing

-- | Render list of sessions with pagination
sessionsWidget :: forall t m . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> Maybe (m (Route t m)) -- ^ Widget for "Back" button
  -> Id Connection -- ^ Connection we want to view sessions to 
  -> m (Route t m)
sessionsWidget token backW conn = do 
  backE <- blueButton "Back"

  --connectE <- fmap (const 0) <$> blueButton "Connect"

  viewE <- renderList (Just 10) renderSession requestSessions

  let thisW = sessionsWidget token backW conn
  let viewR = Route $ eventLogWidget token (Just thisW) <$> viewE
  let backR = Route $ maybe never (\w -> const w <$> backE) backW
  return $ viewR <> backR
  where 

  renderSession :: WithId (Id Session) Session -> m (Event t EventLogId)
  renderSession (WithField _ sess) = elClass "div" "panel panel-default" $ do 
    elClass "div" "panel-body" $ do
      let start = sess ^. rlens (Proxy :: Proxy '("start", UTCTime)) . rfield
          end = sess ^. rlens (Proxy :: Proxy '("end", Maybe UTCTime)) . rfield
          logi = sess ^. rlens (Proxy :: Proxy '("log", EventLogId)) . rfield
      elAttr "span" [("style", "font-weight: bold;")] $ text $ "Start: " <> show start 
        <> " End: " <> show end

      closeE <- case end of 
        Nothing -> blueButton "Disconnect" 
        Just _ -> return never

      viewE <- fmap (const logi) <$> blueButton "View"

      return viewE

  requestSessions :: Event t Page -> m (Event t (Page, PagedList (Id Session) Session))
  requestSessions e = do 
    let mkReq p = (,)
          <$> pure p 
          <*> sessionList (Just p) Nothing (Just conn) (Just (Token token))
    reqEv <- asyncAjax mkReq e
    _ <- widgetHold (pure ()) $ ffor reqEv $ \resp -> case resp of 
      Left er -> danger er 
      Right _ -> return ()    
    let itemsE = either (const $ (0, PagedList [] 0)) id <$> reqEv
    return itemsE 

  danger = elClass "div" "alert alert-danger" . text 