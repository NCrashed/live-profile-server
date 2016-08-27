{-|
Module      : Profile.Live.Server.Client.Pagination
Description : Helpers to make pagination widgets
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLists #-}
module Profile.Live.Server.Client.Pagination(
    renderList
  , renderPager
  ) where

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

renderList :: MonadWidget t m => (WithId (Id a) a -> m ()) -> PagedList (Id a) a -> m ()
renderList render plist = do 
  rec pageDE <- widgetHold (pager 0) $ fmap pager pageE 
      let pageE = switchPromptlyDyn pageDE
  mapM_ render $ pagedListItems plist 
  return ()
  where 
    pager curw = renderPager curw $ pagedListPages plist

renderPager :: forall t m . MonadWidget t m => Page -> Word -> m (Event t Page)
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
