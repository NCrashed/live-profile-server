{-|
Module      : Profile.Live.Server.Client.Bootstrap.Form
Description : Rudimentary support for bootstrap forms
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
module Profile.Live.Server.Client.Bootstrap.Form(
    horizontalForm
  , formGroupText
  ) where 

import Reflex
import Reflex.Dom 

-- | Wrapper for bootstrap horizontal form
horizontalForm :: MonadWidget t m => m a -> m a
horizontalForm = elClass "form" "form-horizontal"

-- | Helper to create bootstrap text input with label
formGroupText :: MonadWidget t m => String -> TextInputConfig t -> m (TextInput t)
formGroupText labelText cfg = formGroup $ do 
  mkLabel [ ("for", elemId)
          , ("class", "col-sm-2 control-label")] $ text labelText
  elClass "div" "col-sm-10" $ textInput cfg {
      _textInputConfig_attributes = constDyn [
          ("class", "form-control")
        , ("id", elemId)]
    }
  where 
    formGroup = elClass "div" "form-group"
    mkLabel = elAttr "label"
    elemId = "input"++labelText
