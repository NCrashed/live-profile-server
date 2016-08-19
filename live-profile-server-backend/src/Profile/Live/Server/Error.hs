module Profile.Live.Server.Error(
    ServerError
  , ErrorType(..)
  , wrapError
  , throw400
  , throw400'
  , throw401
  , throw404
  , throw409
  , throw409'
  , throw429
  , throw429'
  , throw500
  , throw500'
  ) where 

import Control.Monad.Except
import Data.Aeson 
import Data.Swagger 
import Data.Text 
import Data.Text.Encoding 
import Data.Vinyl
import GHC.Generics 
import Servant.API.REST.Derive.Vinyl()
import Servant.Server 

import qualified Data.ByteString.Lazy as BS 

-- | Distinctive code of error type
data ErrorType = 
    Error'FailedResolveAddress
  deriving (Generic, Show, Eq, Read, Enum, Bounded, Ord)

instance FromJSON ErrorType 
instance ToJSON ErrorType 
instance ToSchema ErrorType

-- | Custom error body
type ServerError = FieldRec '[
    '("error", Maybe ErrorType)
  , '("message", Text)
  ]

-- | Wraps servant error in the server format of json body
wrapError :: ServantErr -> ServantErr 
wrapError e = e { errBody = makeBody . decodeUtf8 . BS.toStrict . errBody $ e }

-- | Prepare error response
makeBody :: Text -> BS.ByteString 
makeBody t = encode (Field Nothing :& Field t :& RNil :: ServerError)

-- | Prepare error response
makeBody' :: Text -> ErrorType -> BS.ByteString 
makeBody' t ec = encode (Field (Just ec) :& Field t :& RNil :: ServerError)

throw400 :: MonadError ServantErr m => Text -> m a
throw400 t = throwError $ err400 { errBody = makeBody t }

throw400' :: MonadError ServantErr m => ErrorType -> Text -> m a
throw400' ec t = throwError $ err400 { errBody = makeBody' t ec }

throw401 :: MonadError ServantErr m => Text -> m a
throw401 t = throwError $ err401 { errBody = makeBody t }

throw404 :: MonadError ServantErr m => Text -> m a
throw404 t = throwError $ err404 { errBody = makeBody t }

throw409 :: MonadError ServantErr m => Text -> m a
throw409 t = throwError $ err409 { errBody = makeBody t }

throw409' :: MonadError ServantErr m => ErrorType -> Text -> m a
throw409' ec t = throwError $ err409 { errBody = makeBody' t ec }

throw429 :: MonadError ServantErr m => Text -> m a 
throw429 t = throwError $ ServantErr { 
    errHTTPCode = 429
  , errReasonPhrase = "Too Many Requests"
  , errBody = makeBody t
  , errHeaders = []
  }

throw429' :: MonadError ServantErr m => ErrorType -> Text -> m a 
throw429' ec t = throwError $ ServantErr { 
    errHTTPCode = 429
  , errReasonPhrase = "Too Many Requests"
  , errBody = makeBody' t ec
  , errHeaders = []
  }

throw500 :: MonadError ServantErr m => Text -> m a
throw500 t = throwError $ err500 { errBody = makeBody t }

throw500' :: MonadError ServantErr m => ErrorType -> Text -> m a
throw500' ec t = throwError $ err500 { errBody = makeBody' t ec }

