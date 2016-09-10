{-# LANGUAGE RecursiveDo #-}
module Profile.Live.Server.Client.Auth.Widget(
    authWidget
  , SimpleToken
  ) where 

import Control.Lens 
import Data.Aeson.WithField
import Data.Either
import Data.Maybe 
import Data.Text (pack)
import Reflex.Dom 
import Servant.API.Auth.Token

import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Auth
import Profile.Live.Server.Client.Bootstrap.Form
import Profile.Live.Server.Client.Bootstrap.Modal
import Profile.Live.Server.Client.Utils 

authWidget :: forall t m . MonadWidget t m 
  => Seconds -- ^ Touch token every n seconds
  -> m (Dynamic t (Maybe SimpleToken))
authWidget touchSecs = mdo 
  ev <- getPostBuild
  let cfg = (def :: ModalConfig t) 
        & modalCfgShow .~ leftmost [ev, invalidEvent]
        & modalCfgTitle .~ "Authorise, please (login 'guest', pass '123456789')"
  md <- modal cfg authForm authQuery

  -- repetitive touching 
  let haveTokenEvent = fmap fromJust $ ffilter isJust $ modalValue md
  dynInvalidEvent <- widgetHold (pure never) $ authTouchWidget touchSecs <$> haveTokenEvent
  let invalidEvent = switchPromptlyDyn dynInvalidEvent

  holdDyn Nothing $ modalValue md
  where 
  authQuery i dyna = do  
    -- Setup controls
    cancelEv <- cancelModalBtn "Cancel"
    acceptEv <- acceptModalBtn "Sign in"
    let acceptEv' = dyna `tagDyn` acceptEv
        cancelEv' = fmap (const Nothing) cancelEv

    -- Perform query
    let mkSignin (login, pass) = authSignin (Just login) (Just pass) (Just $ prolongedExpire touchSecs)
    reqEv <- asyncAjax mkSignin acceptEv'
    _ <- widgetHold (pure ()) $ ffor reqEv $ \resp -> case resp of 
      Left er -> danger er 
      Right _ -> return ()    
    let tokenEv = toMaybe . (fmap (\(OnlyField t) -> t)) <$> reqEv

    -- Hide only when the auth is successful 
    modalHideOn i $ ffilter isJust tokenEv
    return $ leftmost [cancelEv', tokenEv]

  toMaybe (Left _) = Nothing
  toMaybe (Right x) = Just x

-- | Converts seconds into expiration duration with special time gap for client to 
-- perform touch request
prolongedExpire :: Seconds -> Seconds
prolongedExpire n = fromIntegral $ n + 10

-- | Make authorisation form that returns login and password
authForm :: MonadWidget t m => m (Dynamic t (Login, Password))
authForm = horizontalForm $ do
  loginInput <- formGroupText "Login" def
  passInput <- formGroupText "Password" def { _textInputConfig_inputType = "password" }
  login <- mapDyn pack $ value loginInput
  pass <- mapDyn pack $ value passInput
  combineDyn (,) login pass

-- | Try to sustain existing token every n seconds, if server invalidates the
-- the token, emits event 
authTouchWidget :: forall t m . MonadWidget t m 
  => Seconds -- ^ Touch token every n seconds
  -> SimpleToken -- ^ Current token
  -> m (Event t ()) -- ^ Our token was invalidated
authTouchWidget n tok = do 
  -- recursive switch. Switch to same delayedTouch when we are done with current one
  rec dynRes <- widgetHold delayedTouch nextEv'
      (dynInvalidEv, dynNextEv) <- splitDyn dynRes -- split events from delayedTouch
      let invalidEv = switchPromptlyDyn dynInvalidEv :: Event t () -- flatten
      let nextEv = switchPromptlyDyn dynNextEv :: Event t () -- flatten
      -- define next step, delay a touch again
      let nextEv' = const delayedTouch <$> nextEv :: Event t (m (Event t (), Event t ()))
  return invalidEv
  where 
  -- takes event, uses it to delay touch request by n seconds and returns pair of events
  -- whether the request is invalid and whether the request successed.
  delayedTouch :: m (Event t (), Event t ())
  delayedTouch = do
    -- Fires when we need to perform next touch
    e <- getPostBuild
    touchEvent <- delay (fromIntegral n) e

    -- Event with ajax touch request
    let mkTouch token = authTouch (Just $ prolongedExpire n) (Just token) 
    reqEv <- asyncAjax (const $ mkTouch $ Token tok) touchEvent

    -- Fires when remote server invalidated our token
    let invalidatedEv = fmap (const ()) $ ffilter isLeft reqEv
    let nextEv = fmap (const ()) $ ffilter isRight reqEv 
    return (traceEvent "invalidated" invalidatedEv, traceEvent "next" nextEv)

-- <form class="form-horizontal">
--   <div class="form-group">
--     <label for="inputEmail3" class="col-sm-2 control-label">Email</label>
--     <div class="col-sm-10">
--       <input type="email" class="form-control" id="inputEmail3" placeholder="Email">
--     </div>
--   </div>
--   <div class="form-group">
--     <label for="inputPassword3" class="col-sm-2 control-label">Password</label>
--     <div class="col-sm-10">
--       <input type="password" class="form-control" id="inputPassword3" placeholder="Password">
--     </div>
--   </div>
--   <div class="form-group">
--     <div class="col-sm-offset-2 col-sm-10">
--       <div class="checkbox">
--         <label>
--           <input type="checkbox"> Remember me
--         </label>
--       </div>
--     </div>
--   </div>
--   <div class="form-group">
--     <div class="col-sm-offset-2 col-sm-10">
--       <button type="submit" class="btn btn-default">Sign in</button>
--     </div>
--   </div>
-- </form>