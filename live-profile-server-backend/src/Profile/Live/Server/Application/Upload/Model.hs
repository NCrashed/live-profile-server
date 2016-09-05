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

import Control.Monad.IO.Class 
import Data.Aeson.WithField
import Data.Maybe 
import Database.Persist.Sql 
import Database.Persist.TH
import System.Directory
import System.Directory.Tree 
import System.FilePath 
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

-- | Delete upload file fby id
deleteUploadFile :: MonadIO m => UploadId -> SqlPersistT m ()
deleteUploadFile = delete . (toKey :: UploadId -> UploadFileImplId)

-- | Check whether the chunk of uploading file already exists
isUploadFileChunkExists :: MonadIO m 
  => FilePath -- ^ Folder that is a storage of chunks
  -> UploadId -- ^ Id of uploading file
  -> ChunkNum -- ^ Number of chunk to check for
  -> m Bool 
isUploadFileChunkExists folder i n = do 
  ns <- getUploadFileChunksNums folder i
  return $ or $ (== n) <$> ns

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
  => FilePath -- ^ Folder that is a storage of chunks
  -> UploadId -- ^ id of uploading
  -> m [FilePath] -- ^ List of files with chunks
getUploadFileChunks folder i = liftIO $ do 
  (_ :/ tree) <- readDirectoryWith (const $ return ()) folder
  case tree of 
    Dir{..} -> return $ catMaybes $ extractFiles <$> contents
    _ -> return []
  where 
  extractFiles d = case d of 
    File{..} -> if show i `L.isPrefixOf` name 
      then Just name 
      else Nothing
    _ -> Nothing

-- | Get list of files that stores chunks for upload file
getUploadFileChunksNums :: MonadIO m 
  => FilePath -- ^ Folder that is a storage of chunks
  -> UploadId -- ^ id of uploading
  -> m [ChunkNum] -- ^ List of files with chunks
getUploadFileChunksNums folder i = do 
  ns <- getUploadFileChunks folder i
  let ns' = catMaybes $ parseChunkFileName <$> ns
  return $ snd <$> ns'

-- | Writing down upload chunk
writeUploadFileChunk :: MonadIO m 
  => FilePath -- ^ Folder that is a storage of chunks
  -> UploadId -- ^ Id of uploading file
  -> ChunkNum -- ^ Number of chunk to check for
  -> BS.ByteString -- ^ Content of chunk 
  -> m ()
writeUploadFileChunk folder i n bs = liftIO $ do 
  let filename = folder </> encodeChunkFileName i n
  BS.writeFile filename bs 

-- | Check wether the upload is finished
isUploadFileFinished :: MonadIO m 
  => FilePath -- ^ Folder that is a storage of chunks
  -> UploadId -- ^ Id of uploading file
  -> SqlPersistT m Bool
isUploadFileFinished folder i = do 
  minfo <- readUploadFileInfo i
  maybe (return False) isFinished minfo
  where 
  isFinished ui@UploadFileInfo{..} = do 
    ns <- getUploadFileChunksNums folder i
    return $ length (L.nub ns) >= uploadFileChunksNum ui

-- | Glue all chunks in single file
finishFileUpload :: MonadIO m 
  => FilePath -- ^ Folder that is a storage of chunks
  -> FilePath -- ^ Folder where a finished file is placed
  -> UploadId -- ^ Id of uploading file
  -> SqlPersistT m ()
finishFileUpload folder destFolder i = do 
  minfo <- readUploadFileInfo i 
  whenJust minfo $ \UploadFileInfo{..} -> do 
    let filename = destFolder </> uploadFileInfoName
    liftIO $ whenM (doesFileExist filename) $ removeFile filename
    ns <- getUploadFileChunks folder i 
    liftIO $ mapM_ (writeChunk filename) ns 
  where 
  writeChunk filename chunkName = do 
    bs <- BS.readFile (folder </> chunkName)
    BS.appendFile filename bs 

-- | Delete all chunks of specific uploading
deleteUploadFileChunks :: MonadIO m 
  => FilePath -- ^ Folder that is a storage of chunks
  -> UploadId -- ^ Id of uploading file to delete
  -> m ()
deleteUploadFileChunks folder i = liftIO $ do 
  ns <- getUploadFileChunks folder i
  mapM_ removeFile ns