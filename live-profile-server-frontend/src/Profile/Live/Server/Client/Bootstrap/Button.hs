{-|
Module      : Profile.Live.Server.Client.Bootstrap.Button
Description : Bootstrap widgets for buttons
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
module Profile.Live.Server.Client.Bootstrap.Button(
    blueButton
  ) where

import Reflex as R
import Reflex.Dom as R

blueButton :: MonadWidget t m => String -> m (Event t ())
blueButton s = do
  (e, _) <- elAttr' "button" [("type", "button")
    , ("class", "btn btn-primary")
    , ("style", "margin: 5px") ] $ text s
  return $ domEvent Click e