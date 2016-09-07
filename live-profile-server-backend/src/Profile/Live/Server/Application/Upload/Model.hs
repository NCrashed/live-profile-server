{-|
Module      : Profile.Live.Server.Application.Upload.Model
Description : Implementation of DB related stuff for Upload API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Profile.Live.Server.Application.Upload.Model where

import Control.DeepSeq
import Control.Monad (void, when)
import Control.Monad.IO.Class 
import Data.Aeson.WithField
import Database.Persist.Sql 
import Database.Persist.TH
import System.Directory
import System.FilePath 
import System.IO 
import Text.Read (readMaybe)

import qualified Data.ByteString as BS 
import qualified Data.List as L 

import Profile.Live.Server.API.Upload
import Profile.Live.Server.Utils 

share [mkPersist sqlSettings
     , mkDeleteCascade sqlSettings
     , mkMigrate "migrateAll"] [persistLowerCase|
UploadFileImpl
  name String 
  type String Maybe 
  size Word 
  chunkSize Word 
  UploadFileUnique name

UploadFileChunk 
  upload UploadFileImplId 
  num ChunkNum
  name FilePath
  UploadFileChunkUnique upload num
|]

-- | Helper to convert into DB representation
toUploadFileImpl :: UploadFileInfo -> UploadFileImpl 
toUploadFileImpl UploadFileInfo{..} = UploadFileImpl {
    uploadFileImplName = uploadFileInfoName
  , uploadFileImplType = uploadFileInfoType
  , uploadFileImplSize = uploadFileInfoSize
  , uploadFileImplChunkSize = uploadFileInfoChunkSize
  }

-- | Helper to convert from DB representation
fromUploadFileImpl :: UploadFileImpl -> UploadFileInfo
fromUploadFileImpl UploadFileImpl{..} = UploadFileInfo {
    uploadFileInfoName = uploadFileImplName
  , uploadFileInfoType = uploadFileImplType
  , uploadFileInfoSize = uploadFileImplSize
  , uploadFileInfoChunkSize = uploadFileImplChunkSize
  }

-- | Read upload file info by id
readUploadFileInfo :: MonadIO m => UploadId -> SqlPersistT m (Maybe UploadFileInfo)
readUploadFileInfo = fmap (fmap fromUploadFileImpl) . get . (toKey :: UploadId -> UploadFileImplId)

-- | Get file upload by file name
getFileUploadByName :: MonadIO m => String -> SqlPersistT m (Maybe (WithId UploadId UploadFileInfo))
getFileUploadByName name = do 
  e <- getBy $ UploadFileUnique name 
  return $ (\(Entity i v) -> WithField (fromKey i) $ fromUploadFileImpl v) <$> e

-- | Create new record about uploading file, filename should be unique
insertUploadFile :: MonadIO m => UploadFileInfo -> SqlPersistT m UploadId
insertUploadFile = fmap fromKey . insert . toUploadFileImpl

-- | Delete upload file by id
deleteUploadFile :: MonadIO m => UploadId -> SqlPersistT m ()
deleteUploadFile i = do
  let i' = toKey i ::  UploadFileImplId
  chunks <- fmap snd <$> getUploadFileChunks i
  liftIO $ mapM_ removeFileIfExists chunks
  deleteCascade i' 
  where 
  removeFileIfExists f = whenM (doesFileExist f) $ removeFile f

-- | Check whether the chunk of uploading file already exists
isUploadFileChunkExists :: MonadIO m 
  => UploadId -- ^ Id of uploading file
  -> ChunkNum -- ^ Number of chunk to check for
  -> SqlPersistT m Bool 
isUploadFileChunkExists i n = do 
  chunks <- count [UploadFileChunkUpload ==. toKey i, UploadFileChunkNum ==. n] 
  return $ chunks /= 0

-- | Extract chunk and upload ids from chunk filename
parseChunkFileName :: FilePath -> Maybe (UploadId, ChunkNum)
parseChunkFileName f = if ext == ".chunk" 
  then (,) <$> readMaybe uis <*> readMaybe cis
  else Nothing
  where 
  (uis, f') = L.break (== '_') f 
  (cis, ext) = L.break (== '.') $ drop 1 f'

-- | Encode chunk and upload ids to chunk filename
encodeChunkFileName :: UploadId -> ChunkNum -> FilePath 
encodeChunkFileName i n = show i ++ "_" ++ show n ++ ".chunk"

-- | Get list of files that stores chunks for upload file
getUploadFileChunks :: MonadIO m 
  => UploadId -- ^ id of uploading
  -> SqlPersistT m [(ChunkNum, FilePath)] -- ^ List of files with chunks
getUploadFileChunks i = do 
  res <- selectList [UploadFileChunkUpload ==. toKey i] [Asc UploadFileChunkNum]
  let getNumAndPath = \(Entity _ UploadFileChunk{..}) -> (uploadFileChunkNum, uploadFileChunkName)
  return $ getNumAndPath <$> res

-- | Get list of files that stores chunks for upload file
getUploadFileChunksNums :: MonadIO m 
  => UploadId -- ^ id of uploading
  -> SqlPersistT m [ChunkNum] -- ^ List of files with chunks
getUploadFileChunksNums  i = do 
  res <- selectList [UploadFileChunkUpload ==. toKey i] [Asc UploadFileChunkNum]
  return $ uploadFileChunkNum . entityVal <$> res

-- | Get list of files that stores chunks for upload file
getUploadFileChunksCount :: MonadIO m 
  => UploadId -- ^ id of uploading
  -> SqlPersistT m Int -- ^ Count of chunks registered
getUploadFileChunksCount i = do 
  count [UploadFileChunkUpload ==. toKey i] 

-- | Writing down upload chunk and put record about it into DB
writeUploadFileChunk :: MonadIO m 
  => FilePath -- ^ Folder that is a storage of chunks
  -> UploadId -- ^ Id of uploading file
  -> ChunkNum -- ^ Number of chunk to check for
  -> BS.ByteString -- ^ Content of chunk 
  -> SqlPersistT m ()
writeUploadFileChunk folder i n bs = do
  let filename = folder </> encodeChunkFileName i n
  liftIO $ BS.writeFile filename bs 
  void . insert $ UploadFileChunk (toKey i) n filename

-- | Check wether the upload is finished
isUploadFileFinished :: MonadIO m 
  => UploadId -- ^ Id of uploading file
  -> SqlPersistT m Bool
isUploadFileFinished i = do 
  minfo <- readUploadFileInfo i
  maybe (return False) isFinished minfo
  where 
  isFinished ui@UploadFileInfo{..} = do 
    ns <- getUploadFileChunksCount i
    when (ns >=  uploadFileChunksNum ui) $ do 
      x <- getUploadFileChunks i
      liftIO $ print x 
    return $ ns >= uploadFileChunksNum ui

-- | Glue all chunks in single file
finishFileUpload :: MonadIO m 
  => FilePath -- ^ Folder where a finished file is placed
  -> UploadId -- ^ Id of uploading file
  -> UploadFileInfo -- ^ Info about upload file
  -> SqlPersistT m ()
finishFileUpload destFolder i UploadFileInfo{..} = do
  filename <- chooseFreeName $ destFolder </> uploadFileInfoName
  ns <- getUploadFileChunks i 
  liftIO $ withFile filename AppendMode $ \h -> do 
    mapM_ (writeChunk h) ns 
  mapM_ deleteChunk ns 
  where
  -- If file exists, choose name like "myfile (42)"
  chooseFreeName filename = liftIO $ do
    isExists <- doesFileExist filename
    if isExists then go 1
      else return filename
    where 
    go :: Int -> IO FilePath
    go n = do
      let filename' = filename ++ " (" ++ show n ++ ")"
      isExists <- doesFileExist filename'
      if isExists then go (n+1)
        else return filename'

  writeChunk h (_, chunkFilename) = do 
    bs <- BS.readFile chunkFilename
    bs `deepseq` BS.hPut h bs 
    removeFile chunkFilename
  deleteChunk (n, _) = do 
    deleteBy $ UploadFileChunkUnique (toKey i) n

-- | Check if chunks of unfinished uploads are deleted and if so,
-- cleanups DB from garbage.
sanitizeFileUploadChunks :: MonadIO m 
  => SqlPersistT m ()
sanitizeFileUploadChunks = do 
  ids <- selectKeysList ([] :: [Filter UploadFileImpl]) []
  mapM_ sanitize ids
  where 
  sanitize i = do 
    chunks <- getUploadFileChunks (fromKey i) 
    chunksExists <- liftIO $ mapM (doesFileExist . snd) chunks
    let notExisted = fmap snd $ filter (not . fst) $ zip chunksExists chunks
    mapM_ (removeChunk i . fst) notExisted

    let allDeleted = and $ fmap not chunksExists
    when allDeleted $ deleteUploadFile (fromKey i) 
  removeChunk i n = do 
    deleteBy $ UploadFileChunkUnique i n