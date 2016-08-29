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
    defaultButton
  , blueButton
  , greenButton
  , infoButton
  , orangeButton
  , redButton
  , buttonGroup
  ) where

import Reflex as R
import Reflex.Dom as R

blueButton :: MonadWidget t m => String -> m (Event t ())
blueButton = bootstrapButton "btn-primary"

defaultButton :: MonadWidget t m => String -> m (Event t ())
defaultButton = bootstrapButton "btn-default"

greenButton :: MonadWidget t m => String -> m (Event t ())
greenButton = bootstrapButton "btn-success"

infoButton :: MonadWidget t m => String -> m (Event t ())
infoButton = bootstrapButton "btn-info"

orangeButton :: MonadWidget t m => String -> m (Event t ())
orangeButton = bootstrapButton "btn-warning"

redButton :: MonadWidget t m => String -> m (Event t ())
redButton = bootstrapButton "btn-danger"

bootstrapButton :: MonadWidget t m => String -> String -> m (Event t ())
bootstrapButton btnType s = do
  (e, _) <- elAttr' "button" [("type", "button")
    , ("class", "btn " ++ btnType)
    ] $ text s
  return $ domEvent Click e

-- | Wraps inner buttons into group
buttonGroup :: MonadWidget t m => m a -> m a 
buttonGroup = elClass "div" "btn-group"