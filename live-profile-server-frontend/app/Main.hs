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
  mapM_ renderConnection cons
  where 
  cons :: [Connection]
  cons = [
      Field "Test con1" :& Field "localhost" :& Field 8242 :& Field Nothing :& RNil
    , Field "Test con2" :& Field "localhost" :& Field 8244 :& Field Nothing :& RNil
    ]

  renderConnection :: Connection -> m ()
  renderConnection (
       Field name 
    :& Field host 
    :& Field port 
    :& Field lastUsed
    :& RNil ) = elClass "div" "panel panel-default" $ do 
      elClass "div" "panel-body" $ do
        elAttr "span" [("style", "font-weight: bold;")] $ text $ T.unpack name
        text $ " (" <> T.unpack host <> ":" <> show port <> ") "
          <> "Last used: " <> show lastUsed
        sessions <- blueButton "Sessions"
        del <- blueButton "Delete"
        return ()

  requestConns :: Event t (a, b) -> m (Event t [WithId (Id Connection) Connection])
  requestConns e = do 
    let mkSignin (login, pass) = connList (Just 0) Nothing (Just (Token token))
    reqEv <- asyncAjax mkSignin e
    widgetHold (pure ()) $ ffor reqEv $ \resp -> case resp of 
      Left er -> danger er 
      Right _ -> return ()    
    let itemsE = either (const []) pagedListItems <$> reqEv
    return itemsE 

  danger = elClass "div" "alert alert-danger" . text 

blueButton :: MonadWidget t m => String -> m (Event t ())
blueButton s = do
  (e, _) <- elAttr' "button" [("type", "button")
    , ("class", "btn btn-primary")
    , ("style", "margin: 5px") ] $ text s
  return $ domEvent Click e
