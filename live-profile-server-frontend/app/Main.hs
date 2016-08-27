{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Aeson.WithField
import Data.Monoid 
import Data.Text (Text)
import Data.Vinyl
import Reflex as R
import Reflex.Dom as R
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Text.Printf

import qualified Data.Text as T 
import qualified Data.Vector as V

import Profile.Live.Server.API.Connection 
import Profile.Live.Server.Client.Async 
import Profile.Live.Server.Client.Auth.Widget
import Profile.Live.Server.Client.Bined
import Profile.Live.Server.Client.Connection
import Profile.Live.Server.Client.Pagination

main :: IO ()
main = mainWidget $ do
  mtok <- authWidget 30
  dyn =<< mapDyn withTokenApp mtok
  return ()
  where 
  withTokenApp :: MonadWidget t m => Maybe SimpleToken -> m ()
  withTokenApp = maybe notAuthWidget connectionsWidget

showt :: Show a => a -> Text 
showt = T.pack . show

notAuthWidget :: MonadWidget t m => m ()
notAuthWidget = el "h1" $ text "Authorise please" 

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

blueButton :: MonadWidget t m => String -> m (Event t ())
blueButton s = do
  (e, _) <- elAttr' "button" [("type", "button")
    , ("class", "btn btn-primary")
    , ("style", "margin: 5px") ] $ text s
  return $ domEvent Click e
