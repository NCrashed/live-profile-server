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

import Profile.Live.Server.API.Upload 
import Profile.Live.Server.Client.Async
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
  , uploadChunkSize = 4 * 1024 -- 4 Kb
  }

-- | Convert upload file to meta info required by server
toUploadFileInfo :: Word -> UploadFile -> UploadFileInfo
toUploadFileInfo chunkSize UploadFile{..} = UploadFileInfo {
    uploadFileInfoName = uploadFileName
  , uploadFileInfoType = Just uploadFileType 
  , uploadFileInfoSize = uploadFileSize 
  , uploadFileInfoChunkSize = chunkSize
  }

-- | Upload widget that uploads given file to server. Returns
-- event that fires when the uploading is finished.
uploadWidget :: forall t m . MonadWidget t m 
  => SimpleToken -- ^ Authorisation token
  -> UploadConfig -- ^ Configuration of the widget
  -> UploadFile -- ^ File we want to upload
  -> m (Event t ())
uploadWidget token UploadConfig{..} uf = do 
  uploadE <- initialCheck (uploadFileName uf)
  rec 
    percentE <- widgetHoldEvent' $ sendingWidget cancelE <$> uploadE
    let cancelE = never
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
      toProgress n = (fromIntegral $ n * uploadFileInfoChunkSize) / 
        (fromIntegral . BS.length $ uploadFileContent uf)

      -- Converts output of chunksWatcher into progress value
      progressWatcher :: m (Event t Double)
      progressWatcher = do 
        chunksE <- chunksWatcher
        return $ toProgress <$> chunksE

      -- Spawns workers for each concurrent chunk upload and collects
      -- total count of chunks uploaded
      chunksWatcher :: m (Event t Word)
      chunksWatcher =  do 
        workerE <- mapM chunkWorker [1 .. uploadConcurrentChunks]
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
        foldEvent (const (+1)) 0 finishedE

      -- Slice file to get required chunk part
      getChunkContents :: ChunkNum -> BS.ByteString -> BS.ByteString
      getChunkContents n = BS.take (fromIntegral uploadFileInfoChunkSize)
        . BS.drop (fromIntegral $ n * uploadFileInfoChunkSize) 

      -- Process single chunk and fire when finished
      processChunk :: ChunkNum -> m (Event t ChunkNum)
      processChunk n = do 
        let bs = getChunkContents n $ uploadFileContent uf
        initialE <- getPostBuild
        checkE <- uploadChunkGetReq (const (i, n) <$> initialE)
        let alreadyE = fforMaybe checkE $ \b -> if b then Just () else Nothing
        let failE = fforMaybe checkE $ \b -> if b then Nothing else Just (i, n, bs)
        uploadedE <- uploadChunkPostReq failE
        return $ leftmost $ fmap (const n) <$> [alreadyE, uploadedE]


