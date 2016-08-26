{-# LANGUAGE TemplateHaskell #-}
module Profile.Live.Server.Client.Bootstrap.Modal(
    modal
  , simpleModal
  , ModalId
  , Modal(..)
  -- * Modal configuration
  , ModalConfig(..)
  , defaultModalCfg
  , modalCfgDismiss
  , modalCfgTitle
  , modalCfgShow
  -- * Simple modal configuration
  , SimpleModalConfig(..)
  , defaultSimpleModalCfg
  , modalSimpleCfgAcceptTitle
  , modalSimpleCfgCancelTitle
  , modalSimpleCfg
  -- * Utils
  , modalShowOn
  , modalHideOn
  , modalShown
  , modalHidden
  , cancelModalBtn
  , acceptModalBtn
  ) where 

import Control.Monad.IO.Class
import Control.Lens.TH
import Data.Default
import Data.Dependent.Map
import Data.IORef 
import Data.JSString (pack)
import GHCJS.Foreign.Callback
import GHCJS.Types
import Reflex.Dom 
import Reflex.Host.Class
import System.IO.Unsafe 

import qualified Data.Map as Map 

foreign import javascript unsafe "$('#'+$1).modal({ backdrop: 'static', keyboard: false });" js_showModal :: JSString -> IO ()
foreign import javascript unsafe "$('#'+$1).modal('hide');" js_hideModal :: JSString -> IO ()
foreign import javascript unsafe "$('#'+$1).on('show.bs.modal',$2);" js_onModalShown :: JSString -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$('#'+$1).off('show.bs.modal',$2);" js_offModalShown :: JSString -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$('#'+$1).on('hide.bs.modal',$2);" js_onModalHidden :: JSString -> Callback (IO ()) -> IO ()
foreign import javascript unsafe "$('#'+$1).off('hide.bs.modal',$2);" js_offModalHidden :: JSString -> Callback (IO ()) -> IO ()

-- | Unique modal id
newtype ModalId = ModalId { unModalId :: String }
  deriving (Eq, Show)

-- | Show given modal
showModal :: MonadIO m => ModalId -> m ()
showModal (ModalId i) = liftIO $ js_showModal $ pack i 

-- | Hide given modal
hideModal :: MonadIO m => ModalId -> m ()
hideModal (ModalId i) = liftIO $ js_hideModal $ pack i 

-- | Bind showing the modal on given event
modalShowOn :: MonadWidget t m => ModalId -> Event t a -> m ()
modalShowOn i e = performEvent_ (const (liftIO $ showModal i) <$> e)

-- | Bind hiding the modal on given event
modalHideOn :: MonadWidget t m => ModalId -> Event t a -> m ()
modalHideOn i e = performEvent_ (const (liftIO $ hideModal i) <$> e)

-- | Holds prerequisites for modal creation
data ModalConfig t = ModalConfig {
  _modalCfgDismiss :: !Bool -- ^ Use dismiss button
, _modalCfgTitle :: !String -- ^ Display modal title
, _modalCfgShow :: Event t () -- ^ When to show the modal
}

$(makeLenses ''ModalConfig)

-- | Default values for modal config
defaultModalCfg :: Reflex t => ModalConfig t
defaultModalCfg = ModalConfig {
    _modalCfgDismiss = True
  , _modalCfgTitle = "Modal title"
  , _modalCfgShow = never
  }

instance Reflex t => Default (ModalConfig t) where 
  def = defaultModalCfg

-- | Holds important values that are needed after a modal creation
data Modal t a = Modal {
  modalId :: !ModalId -- ^ Unique modal id
, modalValue :: !(Event t (Maybe a)) -- ^ Modal output value event, 'Nothing' means that user dismissed the dialog
}

-- | Create bootstrap dialog
modal :: MonadWidget t m => ModalConfig t 
 -> m a -- ^ Modal body
 -> (ModalId -> a -> m (Event t (Maybe b))) -- ^ Modal footer that defines event of dialog success/dismiss
 -> m (Modal t b)
modal ModalConfig{..} bodyWidget footerWidget = do
  i <- genModalId
  let i' = ModalId i
  modalShowOn i' _modalCfgShow
  elAttr "div" (Map.fromList 
    [ ("class", "modal fade")
    , ("tabindex", "-1") 
    , ("role", "dialog")
    , ("id", i)]) $
      elClass "div" "modal-dialog" $
      elClass "div" "modal-content" $ do
        closeEv <- elClass "div" "modal-header" modalHeader 
        a <- elClass "div" "modal-body" bodyWidget 
        ev <- elClass "div" "modal-footer" $ footerWidget i' a
        return $ Modal {
            modalId = i'
          , modalValue = leftmost [closeEv, ev]
          }
  where
  genModalId = do 
    i <- genId
    return $ "modal" ++ show i 

  modalHeader = do 
    closeEv <- if _modalCfgDismiss 
      then do 
        (e, _) <- elAttr' "button" (Map.fromList 
          [ ("type", "button")
          , ("class", "close")
          , ("data-dismiss", "modal")
          , ("aria-label", "Close")] ) $ 
            elAttr "span" (Map.singleton "aria-hidden" "true") $ text "×"
        return $ fmap (const Nothing) $ domEvent Click e
      else return never
    elClass "h4" "modal-title" $ text _modalCfgTitle
    return closeEv 

-- | Generate unique ids
genId :: MonadIO m => m Int 
genId = liftIO $ do 
  i <- readIORef ref 
  modifyIORef' ref (+1)
  return i 
  where 
  ref = unsafePerformIO $ newIORef 0

-- | Holds prerequisites for modal creation
data SimpleModalConfig t = SimpleModalConfig {
  _modalSimpleCfgAcceptTitle :: !String -- ^ Display for OK button
, _modalSimpleCfgCancelTitle :: !String -- ^ Display for Cancel button
, _modalSimpleCfg :: !(ModalConfig t) -- ^ More general config
}

$(makeLenses ''SimpleModalConfig)

-- | Default values for modal config
defaultSimpleModalCfg :: Reflex t => SimpleModalConfig t
defaultSimpleModalCfg = SimpleModalConfig {
    _modalSimpleCfgAcceptTitle = "OK"
  , _modalSimpleCfgCancelTitle = "Cancel"
  , _modalSimpleCfg = defaultModalCfg
  }

instance Reflex t => Default (SimpleModalConfig t) where 
  def = defaultSimpleModalCfg

-- | Create simple modal with "OK" and "Cancel" buttons
simpleModal :: MonadWidget t m => SimpleModalConfig t
  -> m (Dynamic t a) -- ^ Modal body
  -> m (Modal t a)
simpleModal SimpleModalConfig{..} body = modal _modalSimpleCfg body footer 
  where 
  footer i dyna = do 
    cancelEv <- cancelModalBtn _modalSimpleCfgCancelTitle
    acceptEv <- acceptModalBtn _modalSimpleCfgAcceptTitle
    let acceptEv' = fmap Just $ dyna `tagDyn` acceptEv
        cancelEv' = fmap (const Nothing) cancelEv
    modalHideOn i acceptEv'
    return $ leftmost [cancelEv', acceptEv']

-- | Add callback when the modal is shown, returns teardown callback
onModalShown :: MonadIO m => ModalId -> IO () -> m (IO ())
onModalShown (ModalId i) f = liftIO $ do
  let i' = pack i
  c <- syncCallback ContinueAsync f
  js_onModalShown i' c
  return $ do
    js_offModalShown i' c
    releaseCallback c

-- | Add callback when the modal is hidden, returns teardown callback
onModalHidden :: MonadIO m => ModalId -> IO () -> m (IO ())
onModalHidden (ModalId i) f = liftIO $ do
  let i' = pack i
  c <- syncCallback ContinueAsync f
  js_onModalHidden i' c
  return $ do
    js_offModalHidden i' c
    releaseCallback c

-- | Make event that fires when the modal is shown
modalShown :: MonadWidget t m => ModalId -> m (Event t ())
modalShown i = do
  runWithActions <- askRunWithActions
  postGui <- askPostGui
  newEventWithTrigger $ \tr -> onModalShown i $ postGui $ runWithActions [tr :=> pure ()]

-- | Make event that fires when the modal is hidden
modalHidden :: MonadWidget t m => ModalId -> m (Event t ())
modalHidden i = do 
  runWithActions <- askRunWithActions
  postGui <- askPostGui
  newEventWithTrigger $ \tr -> onModalHidden i $ postGui $ runWithActions [tr :=> pure ()]

-- | Help to create modal cancel button
cancelModalBtn :: MonadWidget t m => String -> m (Event t ())
cancelModalBtn title = do
  (e, _) <- elAttr' "button" (Map.fromList 
    [ ("type", "button")
    , ("class", "btn btn-default")
    , ("data-dismiss", "modal")
    ]) $ text title 
  return $ domEvent Click e

-- | Help to create modal accept button
acceptModalBtn :: MonadWidget t m => String -> m (Event t ())
acceptModalBtn title = do 
  (e, _) <- elAttr' "button" (Map.fromList 
    [ ("type", "button")
    , ("class", "btn btn-primary")
    ]) $ text title 
  return $ domEvent Click e

-- Example of generated modal:
-- <div class="modal fade" tabindex="-1" role="dialog">
--   <div class="modal-dialog">
--     <div class="modal-content">
--       <div class="modal-header">
--         <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
--         <h4 class="modal-title">Modal title</h4>
--       </div>
--       <div class="modal-body">
--         <p>One fine body&hellip;</p>
--       </div>
--       <div class="modal-footer">
--         <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
--         <button type="button" class="btn btn-primary">Save changes</button>
--       </div>
--     </div><!-- /.modal-content -->
--   </div><!-- /.modal-dialog -->
-- </div><!-- /.modal -->