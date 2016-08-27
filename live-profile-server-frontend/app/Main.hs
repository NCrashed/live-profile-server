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
import Profile.Live.Server.Client.Bootstrap.Button
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
