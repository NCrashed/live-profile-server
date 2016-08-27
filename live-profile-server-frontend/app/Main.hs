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
  reqE <- fmap (const 0) <$> getPostBuild
  dataE <- requestConns reqE
  let widgetE = renderList <$> dataE
  widgetHold (pure ()) widgetE
  return ()
  where 

  renderList :: PagedList (Id Connection) Connection -> m ()
  renderList plist = do 
    rec pageDE <- widgetHold (pager 0) $ fmap pager pageE 
        let pageE = switchPromptlyDyn pageDE
    mapM_ renderConnection $ pagedListItems plist 
    return ()
    where 
      pager curw = renderPager curw $ pagedListPages plist

  renderPager :: Word -> Word -> m (Event t Page)
  renderPager curw w = do 
    elAttr "nav" navAttrs $ elAttr "div" pagAttrs $ elClass "ul" "pagination" $ do 
      prevE <- prevButton
      pagesE <- mapM pageButton [0 .. w-1]
      nextE <- nextButton
      return $ leftmost $ [prevE, nextE] ++ pagesE
    where
    navAttrs = [("aria-label", "Items navigation"), ("style", "text-align: center;")]
    pagAttrs = [("style", "display: inline-block")]

    prevButton :: m (Event t Page)
    prevButton 
      | curw == 0 = do
        elClass "li" "disabled" $ el "a" $ text "«"
        return never
      | otherwise = el "li" $ do 
        (el, _) <- elAttr' "a" [("href", "#")] $ text "«"
        return $ const (curw-1) <$> domEvent Click el

    nextButton :: m (Event t Page)
    nextButton 
      | curw == w-1 = do 
        elClass "li" "disabled" $ el "a" $ text "»"
        return never
      | otherwise = el "li" $ do 
        (el, _) <- elAttr' "a" [("href", "#")] $ text "»"
        return $ const (curw+1) <$> domEvent Click el

    pageButton :: Page -> m (Event t Page)
    pageButton i 
      | i == curw = do
        elClass "li" "active" $ el "a" $ text (show $ i+1)
        return never
      | otherwise = el "li" $ do 
        (el, _) <- elAttr' "a" [("href", "#")] $ text (show $ i+1)
        return $ const i <$> domEvent Click el

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
