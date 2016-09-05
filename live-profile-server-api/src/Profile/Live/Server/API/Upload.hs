{-|
Module      : Profile.Live.Server.API.Upload
Description : Sub API for uploading huge files on server.
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Profile.Live.Server.API.Upload(
    UploadId
  , UploadFileInfo(..)
  , ChunkNum
  , ChunkBytes(..)
  , UploadAPI
  , uploadAPI
  , uploadOperations
  ) where 

import Control.Lens
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Proxy 
import Data.Swagger
import GHC.Generics 
import Servant.API 
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.Swagger 

import qualified Data.ByteString as BS

import Profile.Live.Server.Utils.DeriveJson
import Profile.Live.Server.Utils.Schema 

-- | Unique id of file that is uploaded or being uploaded
type UploadId = Word 

-- | Numeric order of a chunk
type ChunkNum = Word 

-- | Meta information about file being uploaded
data UploadFileInfo = UploadFileInfo {
  uploadFileInfoName :: !String -- ^ Name of file
, uploadFileInfoType :: !(Maybe String) -- ^ Mime type of file, ex 'text/plain'
, uploadFileInfoSize :: !Word -- ^ Exact size of file
, uploadFileInfoChunkSize :: !Word -- ^ Size of a chunk
} deriving (Generic, Show)

$(deriveJSON (derivePrefix "uploadFileInfo") ''UploadFileInfo)

instance ToSchema UploadFileInfo where 
  declareNamedSchema = genericDeclareNamedSchema $
    schemaOptionsDropPrefix "uploadFileInfo"

-- | Wrapper around raw bytes to generate proper swagger scheme for API
newtype ChunkBytes = ChunkBytes { unChunkBytes :: BS.ByteString }
  deriving (Show, Eq, Generic, MimeRender OctetStream, MimeUnrender OctetStream)

instance ToSchema ChunkBytes where 
  declareNamedSchema _ = do 
    return $ NamedSchema Nothing $ mempty 
      & type_ .~ SwaggerString -- TODO: https://github.com/GetShopTV/swagger2/issues/76

-- | Upload API specification.
--
-- Process of file upload:
--
-- * Client gets `/upload/file` to check whether there was unfinished upload. 
-- If so, it skips the next step as server responds with meta info.
--
-- * Client posts `/upload/file` with meta info about file.
--
-- * Server responds with id of uploading.
--
-- * Client breaks file into chunks with size it sended in meta info (greater than zero).
--
-- * Before sending a chunk, client checks whether the chunk was uploaded previously. 
-- It is done by GET request to `/upload/chunk`. 
--
-- * Client sends chunks, size should be equal to size sended in meta info except of
-- the last chunk.
--
-- * After last chunk sended server glues all chunks together in mere file.
type UploadAPI = "upload" :> (
       "file" 
    :> Capture "filename" String
    :> TokenHeader' '["file-upload"]
    :> Get '[JSON] (WithId UploadId UploadFileInfo)
  :<|> "file" 
    :> ReqBody '[JSON] UploadFileInfo
    :> TokenHeader' '["file-upload"]
    :> Post '[JSON] (OnlyId UploadId)
  :<|> "file"
    :> Capture "uploading" UploadId
    :> TokenHeader' '["file-upload"]
    :> Delete '[JSON] Unit 
  :<|> "chunk"
    :> Capture "uploading" UploadId
    :> Capture "num" ChunkNum
    :> TokenHeader' '["file-upload"]
    :> Get '[JSON] (OnlyField "exists" Bool)
  :<|> "chunk"
    :> Capture "uploading" UploadId
    :> Capture "num" ChunkNum 
    :> ReqBody '[OctetStream] ChunkBytes
    :> TokenHeader' '["file-upload"]
    :> Post '[JSON] Unit
  :<|> "files"
    :> PageParam
    :> PageSizeParam 
    :> TokenHeader' '["file-upload"]
    :> Get '[JSON] (PagedList UploadId UploadFileInfo) 
  :<|> "chunks"
    :> Capture "uploading" UploadId
    :> PageParam
    :> PageSizeParam
    :> TokenHeader' '["file-upload"]
    :> Get '[JSON] (PagedList ChunkNum Unit)
  )

-- | Value to carry type 'EventLogAPI' around
uploadAPI :: Proxy UploadAPI 
uploadAPI = Proxy 

-- | Select only operations of the Upload API
uploadOperations :: Traversal' Swagger Operation
uploadOperations = operationsOf $ toSwagger uploadAPI
