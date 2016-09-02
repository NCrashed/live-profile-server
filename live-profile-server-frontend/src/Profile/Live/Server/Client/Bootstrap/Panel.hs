{-|
Module      : Profile.Live.Server.Client.Bootstrap.Panel
Description : Bootstrap widgets for panels
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
module Profile.Live.Server.Client.Bootstrap.Panel(
    panel 
  , panelBody
  ) where

import Reflex as R
import Reflex.Dom as R

-- | Wrap into panel
panel :: MonadWidget t m => m a -> m a 
panel = elClass "div" "panel panel-default"

-- | Wrap into panel body
panelBody :: MonadWidget t m => m a -> m a
panelBody = elClass "div" "panel-body"