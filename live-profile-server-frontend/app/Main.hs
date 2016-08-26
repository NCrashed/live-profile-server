{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Monoid 
import Data.Text (Text)
import Data.Vinyl
import Reflex as R
import Reflex.Dom as R
import Text.Printf

import qualified Data.Text as T 
import qualified Data.Vector as V

import Profile.Live.Server.Client.Bined
import Profile.Live.Server.API.Connection 

main :: IO ()
main = mainWidget $ connectionsWidget

showt :: Show a => a -> Text 
showt = T.pack . show

connectionsWidget :: forall t m . MonadWidget t m => m ()
connectionsWidget = do 
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

blueButton :: MonadWidget t m => String -> m (Event t ())
blueButton s = do
  (e, _) <- elAttr' "button" [("type", "button")
    , ("class", "btn btn-primary")
    , ("style", "margin: 5px") ] $ text s
  return $ domEvent Click e
