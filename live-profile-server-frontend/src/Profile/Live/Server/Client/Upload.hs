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
    UploadFileConfig(..)
  , UploadFile(..)
  , uploadFileInput
  , debugUploadFile
  ) where

import Control.Exception (finally)
import Control.Monad.IO.Class 
import Data.JSString (unpack)
import Data.Map (Map)
import Data.Monoid 
import GHC.Generics 
import GHCJS.Buffer
import GHCJS.DOM.File (File, getName)
import GHCJS.Foreign.Callback
import GHCJS.Types (JSString, JSVal)
import JavaScript.TypedArray.ArrayBuffer
import Reflex
import Reflex.Dom 

import qualified Data.ByteString as BS 
import qualified Data.Map.Strict as M

import Profile.Live.Server.Client.Utils 

-- | Additional configuration for upload file input widget
data UploadFileConfig t = UploadFileConfig {
    uploadFileInputAttrs :: Dynamic t (Map String String)
  }

-- | Default configuration
defaultUploadFileConfig :: Reflex t => UploadFileConfig t
defaultUploadFileConfig = UploadFileConfig (constDyn mempty)

-- | Info about file being uploaded
data UploadFile = UploadFile {
  uploadFileName :: !String -- ^ Selected file name
, uploadFileType :: !String -- ^ Example: 'text/plain'
, uploadFileSize :: !Word -- ^ Total size of file
, uploadFileContent :: !BS.ByteString -- ^ Bytestring for content
} deriving (Generic)

-- | Typed wrapper around js FileReader object 
newtype FileReader = FileReader JSVal
-- | Typed wrapper around js event passed into load callback of FileReader
newtype OnLoadEvent = OnLoadEvent JSVal

foreign import javascript unsafe "$r = new FileReader();" 
  js_newFileReader :: IO FileReader
foreign import javascript unsafe "$1.onload = $2;" 
  js_readerOnload :: FileReader -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript unsafe "$r = $1.target.result;" 
  js_onLoadEventArrayBuffer :: OnLoadEvent -> IO ArrayBuffer
foreign import javascript unsafe "$1.readAsArrayBuffer($2);"
  js_readAsArrayBuffer :: FileReader -> File -> IO ()

foreign import javascript unsafe "$r = $1.type;" 
  js_fileType :: File -> IO JSString
foreign import javascript unsafe "$r = $1.size;" 
  js_fileSize :: File -> IO Word

-- | Single file input that returns lazy byte string of file content
uploadFileInput :: forall t m . MonadWidget t m => UploadFileConfig t
  -> m (Event t UploadFile)
uploadFileInput UploadFileConfig{..} = do 
  i <- genId
  let inputId = "fileinput" ++ show i 
  attrs <- mapDyn (M.insert "id" inputId) uploadFileInputAttrs
  let cfg = FileInputConfig attrs
  FileInput{..} <- fileInput cfg
  let filesEvent = updated _fileInput_value
  performEventAsync (readUploadFiles <$> filesEvent)
  where 
  readUploadFiles :: [File] -> (UploadFile -> IO ()) ->  WidgetHost m ()
  readUploadFiles files consume = mapM_ (readUploadFile consume) files

  readUploadFile :: (UploadFile -> IO ()) -> File -> WidgetHost m ()
  readUploadFile consume f = liftIO $ do
    reader <- js_newFileReader
    rec c <- syncCallback1 ContinueAsync (onload c)
    js_readerOnload reader c 
    js_readAsArrayBuffer reader f
    where 
    onload c e = finally (releaseCallback c) $ do 
      print ("!!!!!!!!!!!!!!!!!!" :: String)
      name <- getName f 
      ftype <- js_fileType f 
      size <- js_fileSize f
      contentsBuff <- js_onLoadEventArrayBuffer $ OnLoadEvent e 
      consume $ UploadFile {
          uploadFileName = name 
        , uploadFileType = unpack ftype
        , uploadFileSize = size
        , uploadFileContent = toByteString 0 Nothing $ createFromArrayBuffer contentsBuff
        }

-- | Showcase for upload file input widget
debugUploadFile :: forall t m . MonadWidget t m => m ()
debugUploadFile = do 
  fileE <- uploadFileInput defaultUploadFileConfig
  _ <- widgetHold (pure ()) $ renderFile <$> fileE
  return ()
  where 
  renderFile :: UploadFile -> m ()
  renderFile UploadFile{..} = el "div" $ do 
    el "p" $ text $ "Name: " <> uploadFileName
    el "p" $ text $ "Type: " <> uploadFileType
    el "p" $ text $ "Size: " <> show uploadFileSize
    el "p" $ text $ "First bytes: " <> show (BS.take 10 uploadFileContent)
 