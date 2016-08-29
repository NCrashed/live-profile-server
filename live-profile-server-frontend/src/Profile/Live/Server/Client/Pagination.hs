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
  , renderListReload
  , renderPage
  , renderPageReload
  , renderPager
  ) where

import Data.Aeson.WithField
import Reflex as R
import Reflex.Dom as R
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive

-- | Widget that renders remote data as list with pagination controls
renderList :: forall t m a b . MonadWidget t m 
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> (WithId (Id a) a -> m (Event t b)) -- ^ Renderer of item
  -> (Event t Page -> m (Event t (Page, PagedList (Id a) a))) -- ^ Getter of pages
  -> m (Event t b)
renderList maxPages render req = renderListReload maxPages render req never

-- | Widget that renders remote data as list with pagination controls
renderListReload :: forall t m a b c . MonadWidget t m 
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> (WithId (Id a) a -> m (Event t b)) -- ^ Renderer of item
  -> (Event t Page -> m (Event t (Page, PagedList (Id a) a))) -- ^ Getter of pages
  -> Event t c -- ^ Reload event
  -> m (Event t b)
renderListReload maxPages render = renderPageReload maxPages render' 
  where 
  render' _ (PagedList datum _) = do 
    es <- mapM render datum 
    return $ leftmost es

-- | Widget that renders remote data as pages with pagination controls
renderPage :: forall t m a b . MonadWidget t m 
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> (Page -> PagedList (Id a) a -> m (Event t b)) -- ^ Renderer of page
  -> (Event t Page -> m (Event t (Page, PagedList (Id a) a))) -- ^ Getter of pages
  -> m (Event t b)
renderPage maxPages render getPage = renderPageReload maxPages render getPage never 

-- | Widget that renders remote data as pages with pagination controls
renderPageReload :: forall t m a b c . MonadWidget t m 
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> (Page -> PagedList (Id a) a -> m (Event t b)) -- ^ Renderer of page
  -> (Event t Page -> m (Event t (Page, PagedList (Id a) a))) -- ^ Getter of pages
  -> Event t c -- ^ Reload event
  -> m (Event t b)
renderPageReload maxPages render getPage reloadEvent = do 
  initE <- fmap (const 0) <$> getPostBuild
  rec (pageE, es) <- pager $ leftmost [initE, pageE, const 0 <$> reloadEvent]
  return es
  where 
  pager :: Event t Page -> m (Event t Page, Event t b)
  pager curw = do 
    edata <- getPage curw
    dynRes <- widgetHold (pure (never, never)) $ uncurry renderContent <$> edata
    (pageED, esD) <- splitDyn dynRes
    return $ (switchPromptlyDyn pageED, switchPromptlyDyn esD)

  renderContent :: Page -> PagedList (Id a) a -> m (Event t Page, Event t b)
  renderContent curw pl@(PagedList _ w) = do 
    pageE <- renderPager maxPages curw w
    es <- render curw pl
    return (pageE, es)

-- | Display pager widget that reacts to clicking on it
renderPager :: forall t m . MonadWidget t m 
  => Maybe Word -- ^ How much pages to display by one side of current page, 'Nothing' means all
  -> Page -- ^ Current page
  -> Word -- ^ Maximum count of pages
  -> m (Event t Page) -- Returns event of next page requested by user
renderPager maxSide curw pages' = do 
  elAttr "nav" navAttrs $ elAttr "div" pagAttrs $ elClass "ul" "pagination" $ do 
    begE <- beginButton
    prevE <- prevButton
    pagesE <- mapM pageButton pagesRange 
    nextE <- nextButton
    endE <- endButton
    return $ leftmost $ [begE, endE, prevE, nextE] ++ pagesE
  where
  pages = max 1 pages' -- zero pages leads to strange behavior

  navAttrs = [("aria-label", "Items navigation"), ("style", "text-align: center;")]
  pagAttrs = [("style", "display: inline-block")]
  pagesRange = case maxSide of 
    Nothing -> [0 .. pages-1]
    Just n -> [i | i <- [0 .. pages-1], fromIntegral i > mib n, fromIntegral i < mab n]
    where 
      mib, mab :: Word -> Int 
      mib n = fromIntegral curw - fromIntegral n
      mab n = fromIntegral curw + fromIntegral n

  beginButton :: m (Event t Page)
  beginButton 
    | curw == 0 = do
      elClass "li" "disabled" $ el "a" $ text "««"
      return never
    | otherwise = el "li" $ do 
      (e, _) <- elAttr' "a" [("href", "#")] $ text "««"
      return $ const 0 <$> domEvent Click e

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
    | curw == pages-1 = do 
      elClass "li" "disabled" $ el "a" $ text "»"
      return never
    | otherwise = el "li" $ do 
      (e, _) <- elAttr' "a" [("href", "#")] $ text "»"
      return $ const (curw+1) <$> domEvent Click e

  endButton :: m (Event t Page)
  endButton 
    | curw == pages-1 = do 
      elClass "li" "disabled" $ el "a" $ text "»»"
      return never
    | otherwise = el "li" $ do 
      (e, _) <- elAttr' "a" [("href", "#")] $ text "»»"
      return $ const (pages-1) <$> domEvent Click e

  pageButton :: Page -> m (Event t Page)
  pageButton i 
    | i == curw = do
      elClass "li" "active" $ el "a" $ text (show $ i+1)
      return never
    | otherwise = el "li" $ do 
      (e, _) <- elAttr' "a" [("href", "#")] $ text (show $ i+1)
      return $ const i <$> domEvent Click e
