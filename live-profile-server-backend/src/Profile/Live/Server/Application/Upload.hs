{-|
Module      : Profile.Live.Server.Application.Upload
Description : Implementation of Upload API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application.Upload(
    uploadServer
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Unit 
import Data.Aeson.WithField
import Data.Maybe 
import Data.Monoid 
import Database.Persist
import Servant.API 
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.Server 
import Servant.Server.Auth.Token 
import System.Directory

import qualified Data.ByteString as BS

import Profile.Live.Server.API.Upload 
import Profile.Live.Server.Application.Upload.Model 
import Profile.Live.Server.Config
import Profile.Live.Server.Error
import Profile.Live.Server.Monad 
import Profile.Live.Server.Utils

-- | Implementation of 'UploadAPI'
uploadServer :: ServerT UploadAPI App 
uploadServer = getFileEnpoint
  :<|> postFileEndpoint
  :<|> deleteFileEndpoint
  :<|> getChunkEndpoint
  :<|> postChunkEndpoint
  :<|> getFilesEndpoint
  :<|> getChunksEndpoint

getFileEnpoint :: String -- ^ filename
  -> MToken' '["file-upload"] -- ^ Authorisation token
  -> App (WithId UploadId UploadFileInfo) 
getFileEnpoint filename token = do 
  guardAuthToken token 
  runDB404 "uploading" $ getFileUploadByName filename

postFileEndpoint :: UploadFileInfo -- ^ payload with metainfo
  -> MToken' '["file-upload"] -- ^ Authorisation token
  -> App (OnlyId UploadId) 
postFileEndpoint uinfo token = do 
  guardAuthToken token 
  UploadConfig{..} <- getsConfig configUpload
  mres <- runDB $ getFileUploadByName $ uploadFileInfoName uinfo 
  whenJust mres $ const $ 
    throw400' Error'ResourceAlreadyExists "The file with the name is already uploading"
  whenJust uploadConfigMaxFileSize $ \maxSize -> 
    when (uploadFileInfoSize uinfo > maxSize) $
      throw400' Error'FileIsTooLarge "The file is too big"
  runDB $ OnlyField <$> insertUploadFile uinfo 

deleteFileEndpoint :: UploadId -- ^ Id of uploading file to delete
  -> MToken' '["file-upload"] -- ^ Authorisation token
  -> App Unit 
deleteFileEndpoint i token = do 
  guardAuthToken token 
  runDB $ deleteUploadFile i 
  return Unit 

getChunkEndpoint :: UploadId -- ^ Id of uploading file
  -> ChunkNum -- ^ Number of chunk to check for
  -> MToken' '["file-upload"] -- ^ Authorisation token
  -> App (OnlyField "exists" Bool) 
getChunkEndpoint i n token = do 
  guardAuthToken token
  UploadConfig{..} <- getsConfig configUpload
  res <- runDB $ isUploadFileChunkExists i n 
  return $ OnlyField res 

postChunkEndpoint :: UploadId -- ^ Id of uploading file
  -> ChunkNum -- ^ Number of chunk to check for
  -> ChunkBytes -- ^ Chunk binary content
  -> MToken' '["file-upload"] -- ^ Authorisation token
  -> App Unit 
postChunkEndpoint i n chbytes token = do 
  guardAuthToken token 
  UploadConfig{..} <- getsConfig configUpload
  liftIO $ createDirectoryIfMissing True uploadConfigFolder
  ui@UploadFileInfo{..} <- runDB404 "uploading" $ readUploadFileInfo i
  
  let bs = fromChunkBytes chbytes
  when (BS.length bs > fromIntegral uploadFileInfoChunkSize) $ 
    throw400 $ "Wrong chunk size, greater (" <> showt (BS.length bs)
      <> ") than declared (" <> showt uploadFileInfoChunkSize <> ")"
  when (fromIntegral n > uploadFileChunksNum ui) $ 
    throw400 $ "Too much chunks, received " <> showt n <> " is more than needed "
      <> showt (uploadFileChunksNum ui)

  runDB $ writeUploadFileChunk uploadConfigFolder i n bs 

  isFinished <- runDB $ isUploadFileFinished i
  when isFinished $ do 
    liftIO $ createDirectoryIfMissing True uploadConfigSuccess
    runDB $ do
      minfo <- readUploadFileInfo i 
      whenJust minfo $ finishFileUpload uploadConfigSuccess i 
      deleteUploadFile i

  return Unit 

getFilesEndpoint :: Maybe Page -- ^ Requested page
  -> Maybe PageSize -- ^ Requested page size
  -> MToken' '["file-upload"]
  -> App (PagedList UploadId UploadFileInfo)  
getFilesEndpoint mpage msize token = do 
  guardAuthToken token
  pagination mpage msize $ \page size -> do 
    let filters = [] :: [Filter UploadFileImpl]
    (es, total) <- runDB $ (,)
      <$> (do
        (is :: [Key UploadFileImpl]) <- selectKeysList filters [OffsetBy (fromIntegral $ page * size), LimitTo (fromIntegral size)]
        forM is $ (\i -> fmap (WithField i) <$> readUploadFileInfo i) . fromKey)
      <*> count filters
    return PagedList {
        pagedListItems = catMaybes es
      , pagedListPages = ceiling $ (fromIntegral total :: Double) / fromIntegral size
      }

getChunksEndpoint :: UploadId -- ^ uploading id the chunks requested for
  -> Maybe Page -- ^ Requested page
  -> Maybe PageSize -- ^ Requested page size
  -> MToken' '["file-upload"]
  -> App (PagedList ChunkNum Unit) 
getChunksEndpoint  i mpage msize token = do 
  guardAuthToken token 
  UploadConfig{..} <- getsConfig configUpload
  chunkNums <- runDB $ getUploadFileChunksNums i 
  pagination mpage msize $ \page size -> do
    let chunkNums' = take (fromIntegral size) . drop (fromIntegral $ page * size) $ chunkNums
    let total = length chunkNums
    let pages = ceiling $ (fromIntegral total :: Double) / fromIntegral size
    return $ PagedList (flip WithField Unit <$> chunkNums') pages 
