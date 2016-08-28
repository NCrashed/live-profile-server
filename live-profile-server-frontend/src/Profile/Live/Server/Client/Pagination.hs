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
import Reflex as R
import Reflex.Dom as R
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive

renderList :: forall t m a b . MonadWidget t m 
  => (WithId (Id a) a -> m (Event t b)) -- ^ Renderer of item
  -> (Event t Page -> m (Event t (Page, PagedList (Id a) a))) -- ^ Getter of pages
  -> m (Event t b)
renderList render getPage = do 
  initE <- fmap (const 0) <$> getPostBuild
  rec (pageE, es) <- pager $ leftmost [initE, pageE]
  return es
  where 
  pager :: Event t Page -> m (Event t Page, Event t b)
  pager curw = do 
    edata <- getPage curw
    dynRes <- widgetHold (pure (never, never)) $ uncurry renderContent <$> edata
    (pageED, esD) <- splitDyn dynRes
    return $ (switchPromptlyDyn pageED, switchPromptlyDyn esD)

  renderContent :: Page -> PagedList (Id a) a -> m (Event t Page, Event t b)
  renderContent curw (PagedList datum w) = do 
    pageE <- renderPager curw w
    es <- mapM render datum
    return (pageE, leftmost es)

    
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
      (e, _) <- elAttr' "a" [("href", "#")] $ text "«"
      return $ const (curw-1) <$> domEvent Click e

  nextButton :: m (Event t Page)
  nextButton 
    | curw == w-1 = do 
      elClass "li" "disabled" $ el "a" $ text "»"
      return never
    | otherwise = el "li" $ do 
      (e, _) <- elAttr' "a" [("href", "#")] $ text "»"
      return $ const (curw+1) <$> domEvent Click e

  pageButton :: Page -> m (Event t Page)
  pageButton i 
    | i == curw = do
      elClass "li" "active" $ el "a" $ text (show $ i+1)
      return never
    | otherwise = el "li" $ do 
      (e, _) <- elAttr' "a" [("href", "#")] $ text (show $ i+1)
      return $ const i <$> domEvent Click e
