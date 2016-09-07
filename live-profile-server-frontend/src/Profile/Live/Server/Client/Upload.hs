{-|
Module      : Profile.Live.Server.Client.Upload
Description : Uploading large files to server
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Profile.Live.Server.Client.Upload(
  -- * Server API
    uploadFileGet
  , uploadFilePost
  , uploadFileDelete
  , uploadChunkGet
  , uploadChunkPost
  , uploadFiles
  , uploadChunks
  -- * Uploads
  , UploadConfig(..)
  , defaultUploadConfig
  , uploadWidget
  ) where

import Control.Monad.Trans.Either
import Data.Aeson.Unit 
import Data.Aeson.WithField 
import Data.Foldable 
import Data.Monoid 
import Data.Proxy 
import Data.Text (Text)
import GHCJS.Marshal
import Reflex as R
import Reflex.Dom as R
import Servant.API
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.Client 

import qualified Data.ByteString as BS 
import qualified Data.Map as M 

import Profile.Live.Server.API.Upload 
import Profile.Live.Server.Client.Async
import Profile.Live.Server.Client.Bootstrap.Button
import Profile.Live.Server.Client.Bootstrap.Form
import Profile.Live.Server.Client.Bootstrap.Modal
import Profile.Live.Server.Client.Bootstrap.Panel
import Profile.Live.Server.Client.Bootstrap.Progress
import Profile.Live.Server.Client.Upload.Input
import Profile.Live.Server.Client.Utils

-- | Get meta info about existing uploading
uploadFileGet :: String -- ^ File name
  -> MToken' '["file-upload"]
  -> EitherT ServantError IO (WithId UploadId UploadFileInfo)

-- | Post new uploading
uploadFilePost :: UploadFileInfo
  -> MToken' '["file-upload"]
  -> EitherT ServantError IO (OnlyId UploadId)

-- | Delete existing uploading
uploadFileDelete :: UploadId
  -> MToken' '["file-upload"]
  -> EitherT ServantError IO Unit

-- | Check if chunk already uploaded
uploadChunkGet :: UploadId
  -> ChunkNum
  -> MToken' '["file-upload"]
  -> EitherT ServantError IO (OnlyField "exists" Bool)

-- | Upload chunk to server
uploadChunkPost :: UploadId
  -> ChunkNum
  -> ChunkBytes
  -> MToken' '["file-upload"]
  -> EitherT ServantError IO Unit

-- | Get list of uploadings
uploadFiles :: Maybe Page 
  -> Maybe PageSize
  -> MToken' '["file-upload"]
  -> EitherT ServantError IO (PagedList UploadId UploadFileInfo)

-- | Get list of chunks already uploaded
uploadChunks :: UploadId
  -> Maybe Page 
  -> Maybe PageSize
  -> MToken' '["file-upload"]
  -> EitherT ServantError IO (PagedList ChunkNum Unit)


(      uploadFileGet
  :<|> uploadFilePost
  :<|> uploadFileDelete
  :<|> uploadChunkGet
  :<|> uploadChunkPost
  :<|> uploadFiles 
  :<|> uploadChunks 
    ) = client uploadAPI Nothing

instance ToJSVal UploadFileInfo where 
  toJSVal = toJSVal_aeson
instance ToJSVal ChunkBytes where 
  toJSVal = toJSVal_aeson

-- | Configuration of uploading widget
data UploadConfig = UploadConfig {
  uploadChunkSize :: Word -- ^ Size of chunk the file is broken to
, uploadConcurrentChunks :: Word -- ^ How many chunks are uploading simultaneously
}

-- | Default configurations for uploading widget
defaultUploadConfig :: UploadConfig
defaultUploadConfig = UploadConfig {
    uploadConcurrentChunks = 5
  , uploadChunkSize = 1024 * 1024 -- 1 Mb
  }

-- | Convert upload file to meta info required by server
toUploadFileInfo :: Word -> UploadFile t m -> UploadFileInfo
toUploadFileInfo chunkSize UploadFile{..} = UploadFileInfo {
    uploadFileInfoName = uploadFileName
  , uploadFileInfoType = Just uploadFileType 
  , uploadFileInfoSize = uploadFileSize 
  , uploadFileInfoChunkSize = chunkSize
  }

-- | Full widget with modal and support for multiple file uploading
uploadWidget :: forall t m a . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> UploadConfig -- ^ Configuration of the widget
  -> Event t a -- ^ When to show modal to user
  -> m (Event t ()) -- ^ Fires when an upload was finished 
uploadWidget token cfg showE = do 
  uploadE <- uploadChooserModal showE
  widgetHoldEvent' $ uploadFileWidget token cfg <$> uploadE

  -- TODO: Below is failed attempt to handler dynamic list of uploadings 
  -- uploadE <- uploadChooserModal showE
  -- rec
  --   let mapActionE = leftmost [
  --           Left <$> uploadE
  --         , Right <$> cancelMapE
  --         ]
  --   dynUploadMap <- foldDyn addOrRemove mempty mapActionE
  --   traceDynWidget dynUploadMap
  --   dynCancelMap <- list dynUploadMap singleUpload 
  --   dynCancelMap' <- mergeMap `mapDyn` dynCancelMap
  --   let cancelMapE = switchPromptlyDyn dynCancelMap'

  -- return $ const () <$> cancelMapE
  -- where
  -- addOrRemove :: Either UploadFile (M.Map String ()) -> M.Map String UploadFile -> M.Map String UploadFile
  -- addOrRemove v m = case v of 
  --   Left uf -> M.insert (uploadFileName uf) uf m
  --   Right del -> foldl' (flip M.delete) m $ M.keys del

  -- singleUpload :: Dynamic t UploadFile -> m (Event t ())
  -- singleUpload dynFile = do 
  --   dynUploader <- uploadFileWidget token cfg `mapDyn` dynFile
  --   cancelE <- dyn dynUploader
  --   return $ coincidence cancelE

-- | Widget with modal to choose file from user file system
uploadChooserModal :: forall t m a. MonadWidget t m 
  => Event t a -- ^ When to show modal to user
  -> m (Event t (UploadFile t m))
uploadChooserModal openE = do 
  modal <- simpleModal cfg body
  return $ fforMaybe (modalValue modal) id
  where 
  cfg = defaultSimpleModalCfg 
    & modalCfg . modalCfgShow .~ fmap (const ()) openE
    & modalCfg . modalCfgTitle .~ "Choose .eventlog file"
  body = horizontalForm $ do
    uploadE <- uploadFileInput defaultUploadFileConfig
    let defFile = UploadFile {
            uploadFileName = ""
          , uploadFileType = ""
          , uploadFileSize = 0
          , uploadFileContent = const $ return never 
          }
    holdDyn defFile uploadE

-- | Upload widget that uploads given file to server. Returns
-- event that fires when the uploading is finished.
uploadFileWidget :: forall t m . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> UploadConfig -- ^ Configuration of the widget
  -> UploadFile t m -- ^ File we want to upload
  -> m (Event t ())
uploadFileWidget token UploadConfig{..} uf = do 
  uploadE <- initialCheck (uploadFileName uf)
  rec 
    percentE <- widgetHoldEvent' $ sendingWidget cancelE <$> uploadE
    cancelE <- renderUpload percentE
  return $ fforMaybe percentE $ \p -> if p `approxEq` 1.0 then Just () else Nothing
  where 
    uploadFileGetReq :: Event t String -> m (Event t (Either String (WithId UploadId UploadFileInfo)))
    uploadFileGetReq = asyncAjax (\f -> uploadFileGet f (Just $ Token token))

    uploadFilePostReq :: Event t UploadFileInfo -> m (Event t (WithId UploadId UploadFileInfo))
    uploadFilePostReq e = simpleRequest e $ \ufi -> do 
      OnlyField i <- uploadFilePost ufi (Just $ Token token)
      return $ WithField i ufi

    uploadChunkGetReq :: Event t (UploadId, ChunkNum) -> m (Event t Bool)
    uploadChunkGetReq e = simpleRequest e $ \(i, n) -> do 
      OnlyField r <- uploadChunkGet i n (Just $ Token token)
      return r 

    uploadChunkPostReq :: Event t (UploadId, ChunkNum, BS.ByteString) -> m (Event t ())
    uploadChunkPostReq e = simpleRequest e $ \(i, n, bs) -> do 
      uploadChunkPost i n (toChunkBytes bs) (Just $ Token token)
      return ()

    -- Check if the upload wasn't finished perviously, if not finished returns meta of old
    -- upload, else starts new upload.
    initialCheck :: String -> m (Event t (WithId UploadId UploadFileInfo))
    initialCheck fname = do 
      initialE <- getPostBuild
      oldE <- uploadFileGetReq (const fname <$> initialE)
      let oldSuccE = fforMaybe oldE $ either (const Nothing) Just
      let oldFailE = fforMaybe oldE $ either Just (const Nothing)

      let newUploadE = const (toUploadFileInfo uploadChunkSize uf) <$> oldFailE
      newUploadedE <- uploadFilePostReq newUploadE

      return $ leftmost [oldSuccE, newUploadedE]

    -- Sends chunks to server and returns updates of progress in 0 .. 1 range
    sendingWidget :: forall a . Event t a -- ^ Cancel upload event
      -> WithId UploadId UploadFileInfo -- ^ Meta info about upload
      -> m (Event t Double) -- ^ Returns progress of upload
    sendingWidget cancelE (WithField i ui@UploadFileInfo{..}) = do 
      widgetHoldEvent progressWatcher $ const (return never) <$> cancelE
      where
      totalChunks = uploadFileChunksNum ui 

      -- Convert number of chunks to percent of file
      toProgress :: Word -> Double
      toProgress n = (fromIntegral n) / (fromIntegral totalChunks)

      -- Converts output of chunksWatcher into progress value
      progressWatcher :: m (Event t Double)
      progressWatcher = do 
        chunksE <- chunksWatcher
        return $ toProgress <$> chunksE

      -- Spawns workers for each concurrent chunk upload and collects
      -- total count of chunks uploaded
      chunksWatcher :: m (Event t Word)
      chunksWatcher =  do 
        let maxWorkers = min (fromIntegral totalChunks) uploadConcurrentChunks
        workerE <- mapM chunkWorker [0 .. maxWorkers-1]
        foldEvent (+) 0 $ mergeWith (+) workerE

      -- Process single chunk worker and returns event that fires event 
      -- with number of chunks processed.
      chunkWorker :: ChunkNum -> m (Event t Word)
      chunkWorker n = do 
        startE <- fmap (const n) <$> getPostBuild
        rec 
          finishedE <- widgetHoldEvent' $ processChunk <$> leftmost [nextE, startE]
          let nextE = fforMaybe finishedE $ \n -> let 
                n' = n + uploadConcurrentChunks
                in if n' > fromIntegral totalChunks then Nothing 
                   else Just n'
        return $ const 1 <$> finishedE

      -- Slice file to get required chunk part
      getChunkContents :: ChunkNum -> m (Event t BS.ByteString)
      getChunkContents n = do 
        initE <- getPostBuild
        let chunkBounds = (n * uploadFileInfoChunkSize, (n + 1) * uploadFileInfoChunkSize)
        uploadFileContent uf $ const chunkBounds <$> initE

      -- Process single chunk and fire when finished
      processChunk :: ChunkNum -> m (Event t ChunkNum)
      processChunk n = do 
        chunkE <- getChunkContents n
        widgetHoldEvent' $ ffor chunkE $ \bs -> do 
          initialE <- getPostBuild
          checkE <- uploadChunkGetReq (const (i, n) <$> initialE)
          let alreadyE = fforMaybe checkE $ \b -> if b then Just () else Nothing
          let failE = fforMaybe checkE $ \b -> if b then Nothing else Just (i, n, bs)
          uploadedE <- uploadChunkPostReq failE
          return $ leftmost $ fmap (const n) <$> [alreadyE, uploadedE]

    -- | Render upload progress and return event that fires when user wants to cancel
    -- the upload
    renderUpload :: Event t Double -> m (Event t ())
    renderUpload progressE = panel . panelBody $ do
      el "p" $ elAttr "span" [("style", "font-weight: bold;")] $ 
        text $ "Uploading " ++ uploadFileName uf
      _ <- percentProgressBarEv progressE ProgressInfo False
      redButton "Cancel"

