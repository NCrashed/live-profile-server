{-|
Module      : Profile.Live.Server.Client.Session
Description : Bindings to server API and widgets for sessions
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
module Profile.Live.Server.Client.Session(
  -- * Server API
    sessionGet
  , sessionList
  , sessionConnect
  , sessionDisconnect
  , sessionLocalImport
  -- * Widgets
  , sessionsWidget
  ) where 

import Control.Lens
import Control.Monad.Trans.Either
import Data.Aeson.Unit 
import Data.Aeson.WithField 
import Data.List (sort)
import Data.Monoid 
import Data.Proxy 
import Data.Text (Text)
import Data.Time
import Data.Vinyl
import Reflex as R
import Reflex.Dom as R
import Servant.API
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Client 

import Profile.Live.Server.API.Connection
import Profile.Live.Server.API.EventLog
import Profile.Live.Server.API.Session
import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Bined
import Profile.Live.Server.Client.Bootstrap.Button
import Profile.Live.Server.Client.Bootstrap.Modal
import Profile.Live.Server.Client.Bootstrap.Panel
import Profile.Live.Server.Client.Bootstrap.Progress
import Profile.Live.Server.Client.EventLog
import Profile.Live.Server.Client.Pagination
import Profile.Live.Server.Client.Router
import Profile.Live.Server.Client.Utils

type SessPerm s = MToken '[ 'PermConcat ( 'PermLabel s) ( 'PermLabel "session")]

sessionGet :: Id Session
  -> SessPerm "read-"
  -> EitherT ServantError IO Session

sessionDelete :: Id Session 
  -> MToken' '["delete-session"]
  -> EitherT ServantError IO Unit 

sessionList :: Maybe Page 
  -> Maybe PageSize 
  -> Maybe (Id Connection)
  -> MToken' '["read-session"]
  -> EitherT ServantError IO (PagedList (Id Session) Session)

sessionConnect :: Id Connection
  -> MToken' '["connect-session"]
  -> EitherT ServantError IO (OnlyId (Id Session))

sessionDisconnect :: Id Session 
  -> MToken' '["connect-session"]
  -> EitherT ServantError IO Unit

-- | Initiate server side import from folder
sessionLocalImport :: Id Connection 
  -> MToken' '["write-session"]
  -> EitherT ServantError IO Unit 

(      sessionGet
  :<|> sessionDelete
  :<|> sessionList
  :<|> sessionConnect
  :<|> sessionDisconnect
  :<|> sessionLocalImport
    ) = client sessionAPI Nothing

-- | Actions that used internaly in widget
data SessionAction = 
    SessionViewLog EventLogId
  | SessionViewBined EventLogId
  | SessionDisconnect (Id Session)
  | SessionDelete (Id Session)
  deriving (Eq, Show)

getSessionViewLog :: SessionAction -> Maybe EventLogId
getSessionViewLog a = case a of 
  SessionViewLog i -> Just i 
  _ -> Nothing 

getSessionViewBined :: SessionAction -> Maybe EventLogId
getSessionViewBined a = case a of 
  SessionViewBined i -> Just i 
  _ -> Nothing 

getSessionDisconnect :: SessionAction -> Maybe (Id Session)
getSessionDisconnect a = case a of 
  SessionDisconnect i -> Just i 
  _ -> Nothing 

getSessionDelete :: SessionAction -> Maybe (Id Session)
getSessionDelete a = case a of 
  SessionDelete i -> Just i 
  _ -> Nothing 

-- | Actions that used internally in import widget
data ImportAction = 
    ImportDelete EventLogId 
  | ImportCancel EventLogId 
  deriving (Eq, Show)

getImportDelete :: ImportAction -> Maybe EventLogId
getImportDelete a = case a of 
  ImportDelete i -> Just i 
  _ -> Nothing 

getImportCancel :: ImportAction -> Maybe EventLogId
getImportCancel a = case a of 
  ImportCancel i -> Just i 
  _ -> Nothing 

-- | Render list of sessions with pagination
sessionsWidget :: forall t m . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> Maybe (m (Route t m)) -- ^ Widget for "Back" button
  -> Id Connection -- ^ Connection we want to view sessions to 
  -> m (Route t m)
sessionsWidget token backW conn = do 
  header "Sessions"
  (backE, connectE, locImportE) <- centered $ buttonGroup $ do 
    backE <- blueButton "Back"
    connectE <- fmap (const conn) <$> blueButton "Connect"
    locImportE <- fmap (const conn) <$> blueButton "Local import"
    return (backE, connectE, locImportE)

  connectedE <- connectRequest connectE 
  importedE <- localImportRequest locImportE

  importChangeE <- importWidget

  rec 
    let reloadE = leftmost [
            const () <$> connectedE
          , const () <$> disconnectedE
          , const () <$> deletedE
          , importChangeE
          , importedE]
    sessEvent <- renderListReload (Just 10) renderSession requestSessions reloadE

    let disconnectE = fmapMaybe getSessionDisconnect sessEvent
    disconnectedE <- disconnectRequest disconnectE 

    let deleteE = fmapMaybe getSessionDelete sessEvent
    deletedE <- deleteRequest deleteE

  let viewLogE = fmapMaybe getSessionViewLog sessEvent
  let binedE = fmapMaybe getSessionViewBined sessEvent

  let thisW = sessionsWidget token backW conn
  let viewLogR = Route $ eventLogWidget token (Just thisW) <$> viewLogE
  let viewBinedR = Route $ binedGraphWidget token (Just thisW) <$> binedE
  let backR = Route $ maybe never (\w -> const w <$> backE) backW
  return $ viewLogR <> backR <> viewBinedR
  where 

  renderSession :: WithId (Id Session) Session -> m (Event t SessionAction)
  renderSession (WithField sid sess) = panel $ do 
    panelBody $ do
      let start = sess ^. rlens (Proxy :: Proxy '("start", UTCTime)) . rfield
          end = sess ^. rlens (Proxy :: Proxy '("end", Maybe UTCTime)) . rfield
          logi = sess ^. rlens (Proxy :: Proxy '("log", EventLogId)) . rfield
          merr = sess ^. rlens (Proxy :: Proxy '("error", Maybe Text)) . rfield
      el "p" $
        elAttr "span" [("style", "font-weight: bold;")] $ text $ "Start: " <> show start 
      el "p" $ 
        elAttr "span" [("style", "font-weight: bold;")] $ text $ "End: " <> show end
      whenJust merr $ \err -> el "p" $ 
        elAttr "span" [("style", "font-weight: bold; color: rgb(234,67,53)")] $ 
          text $ "Error: " <> show err 

      buttonGroup $ do 
        closeE <- case end of 
          Nothing -> fmap (const $ SessionDisconnect sid) <$> blueButton "Disconnect" 
          Just _ -> return never
        viewE <- fmap (const $ SessionViewLog logi) <$> infoButton "View"
        binedE <- fmap (const $ SessionViewBined logi) <$> infoButton "Graphic"
        delE <- confirm def =<< (fmap (const $ SessionDelete sid) <$> redButton "Delete")
        return $ leftmost [viewE, closeE, binedE, delE]


  requestSessions :: Event t Page -> m (Event t (Page, PagedList (Id Session) Session))
  requestSessions e = simpleRequest e $ \p -> (,)
    <$> pure p 
    <*> sessionList (Just p) Nothing (Just conn) (Just (Token token))

  connectRequest :: Event t (Id Connection) -> m (Event t (Id Session))
  connectRequest e = simpleRequest e $ \conn -> do 
    OnlyField i <- sessionConnect conn (Just (Token token))
    return i

  localImportRequest :: Event t (Id Connection) -> m (Event t ())
  localImportRequest e = simpleRequest e $ \conn -> do 
    _ <- sessionLocalImport conn (Just (Token token))
    return ()

  disconnectRequest :: Event t (Id Session) -> m (Event t (Id Session))
  disconnectRequest e = simpleRequest e $ \sess -> do 
    _ <- sessionDisconnect sess (Just (Token token))
    return sess

  deleteRequest :: Event t (Id Session) -> m (Event t (Id Session))
  deleteRequest e = simpleRequest e $ \sess -> do 
    _ <- sessionDelete sess (Just (Token token))
    return sess

  importingRequest :: Event t a -> m (Event t [EventLogImport])
  importingRequest e = simpleRequest e $ const $ 
    importingList (Just (Token token))

  importingCancelRequest :: Event t EventLogId -> m (Event t EventLogId)
  importingCancelRequest e = simpleRequest e $ \i -> do 
    _ <- importingCancel i (Just (Token token))
    return i

  eventLogDeleteRequest :: Event t EventLogId -> m (Event t EventLogId)
  eventLogDeleteRequest e = simpleRequest e $ \i -> do 
    _ <- deleteEventLog i (Just (Token token))
    return i

  -- Display logs that are importing now
  -- Return event that signals about need of update of a session widget
  importWidget :: m (Event t ())
  importWidget = do 
    initialE <- getPostBuild
    updateE <- periodical (fromIntegral (5 :: Int) :: NominalDiffTime)

    rec 
      importDataE <- importingRequest $ leftmost [
          const () <$> canceledE
        , const () <$> deletedE
        , updateE
        , initialE] 
      actionE <- fmap switchPromptlyDyn $ 
        widgetHold (pure never) $ renderImports <$> importDataE

      let cancelE = fmapMaybe getImportCancel actionE
      canceledE <- importingCancelRequest cancelE

      let deleteE = fmapMaybe getImportDelete actionE
      deletedE <- importingCancelRequest deleteE

      changedE <- whenPrev idsChanged importDataE
    return $ leftmost [
        const () <$> canceledE
      , const () <$> deletedE
      , const () <$> changedE
      ]
    where 
    idsChanged :: [EventLogImport] -> [EventLogImport] -> Bool 
    idsChanged as bs = ids as /= ids bs 
      where 
      ids = sort . fmap eventLogImportId

    renderImports :: [EventLogImport] -> m (Event t ImportAction)
    renderImports es = do 
      elAttr "div" [("style", "margin-top: 10px;")] $ return ()
      cancelEvents <- mapM renderImport es 
      return $ leftmost cancelEvents

    renderImport :: EventLogImport -> m (Event t ImportAction)
    renderImport EventLogImport{..} = panel . panelBody $ do
      el "p" $ elAttr "span" [("style", "font-weight: bold;")] $ 
        text $ "Importing " ++ eventLogImportFileName
      _ <- progressBar $ constPercentProgress eventLogImportPercent ProgressInfo True
      case eventLogImportError of 
        Just err -> do
          el "p" $ 
            elAttr "span" [("style", "font-weight: bold; color: rgb(234,67,53)")] $ 
              text $ "Error: " <> show err 
          fmap (const $ ImportDelete eventLogImportId) <$> redButton "Delete"
        Nothing -> do 
          e <- redButton "Cancel"
          return $ const (ImportCancel eventLogImportId) <$> e




