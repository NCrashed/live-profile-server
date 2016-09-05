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
module Profile.Live.Server.Client.Upload(
    uploadFileGet
  , uploadFilePost
  , uploadFileDelete
  , uploadChunkGet
  , uploadChunkPost
  , uploadFiles
  , uploadChunks
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

import Profile.Live.Server.API.Upload 
import Profile.Live.Server.Client.Async

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