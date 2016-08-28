{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Profile.Live.Server.Client.Async(
    fromJSVal_aeson
  , printAjaxErr
  , asyncAjax
  , simpleRequest
  ) where 

import Control.Monad.Trans.Either 
import Data.Aeson.Unit 
import Data.Aeson.WithField
import GHC.TypeLits 
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.Client 

import GHCJS.Marshal
import GHCJS.Types
import Data.Aeson as A
import Data.Vinyl

import Reflex
import Reflex.Dom
import Control.Monad.IO.Class 
import Control.Concurrent
import Data.Bifunctor

-- | Helper to implement 'FromJSVal' via aeson instance
fromJSVal_aeson :: FromJSON a => JSVal -> IO (Maybe a)
fromJSVal_aeson v = do
  jsn <- fromJSVal v
  return $ parse =<< jsn
  where
  parse jsn = case fromJSON jsn of 
    A.Error _ -> Nothing 
    Success a -> Just a 

printAjaxErr :: ServantError -> String 
printAjaxErr err = case err of 
  FailureResponse {..} -> "Failure: " ++ show responseStatus
  DecodeFailure {..} -> decodeError
  UnsupportedContentType {..} -> "Unsupported content type"
  InvalidContentTypeHeader {..} -> "Invalid content type header"

asyncAjax :: MonadWidget t m => (a -> EitherT ServantError IO b) -> Event t a -> m (Event t (Either String b))
asyncAjax action e = performEventAsync $ ffor e $ \a cb -> do
  resp <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO $ putMVar resp =<< runEitherT (action a)
  _ <- liftIO $ forkIO $ cb . first printAjaxErr =<< takeMVar resp
  return ()

-- | Helper to request server
--
-- Prints errors in "danger" bootstrap panel.
simpleRequest :: MonadWidget t m 
  => Event t a -- ^ Input arguments
  -> (a -> EitherT ServantError IO b) -- ^ Request function
  -> m (Event t b) -- ^ Result
simpleRequest ea req = do
  reqEv <- asyncAjax req ea
  _ <- widgetHold (pure ()) $ ffor reqEv $ \resp -> case resp of 
    Left er -> danger er 
    Right _ -> return ()    
  return $ fforMaybe reqEv $ either (const Nothing) Just
  where 
  danger = elClass "div" "alert alert-danger" . text 

instance ToJSVal (Id a) where 
  toJSVal = toJSVal_aeson

instance FromJSVal (Id a) where 
  fromJSVal = fromJSVal_aeson

instance ToJSON (FieldRec fields) => ToJSVal (FieldRec fields) where 
  toJSVal = toJSVal_aeson

instance FromJSON (FieldRec fields) => FromJSVal (FieldRec fields) where 
  fromJSVal = fromJSVal_aeson

instance ToJSVal Unit where 
  toJSVal = toJSVal_aeson

instance FromJSVal Unit where 
  fromJSVal = fromJSVal_aeson

instance (ToJSON a, ToJSON b, KnownSymbol f) => ToJSVal (WithField f a b) where 
  toJSVal = toJSVal_aeson

instance (FromJSON a, FromJSON b, KnownSymbol f) => FromJSVal (WithField f a b) where 
  fromJSVal = fromJSVal_aeson

instance (ToJSON a, ToJSON b) => ToJSVal (WithFields a b) where 
  toJSVal = toJSVal_aeson

instance (FromJSON a, FromJSON b) => FromJSVal (WithFields a b) where 
  fromJSVal = fromJSVal_aeson

instance (ToJSON a, KnownSymbol f) => ToJSVal (OnlyField f a) where 
  toJSVal = toJSVal_aeson

instance (FromJSON a, KnownSymbol f) => FromJSVal (OnlyField f a) where 
  fromJSVal = fromJSVal_aeson

instance (ToJSON a, ToJSON b) => ToJSVal (PagedList a b) where 
  toJSVal = toJSVal_aeson

instance (FromJSON a, FromJSON b) => FromJSVal (PagedList a b) where 
  fromJSVal = fromJSVal_aeson