{-|
Module      : Profile.Live.Server.Config.Auth
Description : Configuration of authorisation plugin
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Config.Auth(
    AuthSettings(..)
  , makeAuthConfig
  ) where

import Data.Monoid 
import Database.Persist.Sql (ConnectionPool)
import GHC.Generics 
import Servant.Server.Auth.Token.Config 
import Data.Text (Text)

import qualified Data.Text as T 

import Profile.Live.Server.Utils
import Profile.Live.Server.Utils.DeriveJson 

-- | Authorisation plugin configuration that 
-- can be serialised into settings file.
data AuthSettings = AuthSettings {
  authSettingsExpire :: !Word -- ^ Amount of seconds when token becomes invalid
, authSettingsMaximumExpire :: !(Maybe Word) -- ^ Amount of seconds of token expiration that a client can ask for
, authSettingsRestoreExpire :: !Word -- ^ Amount of seconds of restore codes expiration
, authSettingsPasswordsStrength :: !Int -- ^ For authorisation, defines amount of hashing of new user passwords (should be greater or equal 14). The passwords hashed 2^strength times. It is needed to prevent almost all kinds of brute force attacks, rainbow tables and dictionary attacks.
, authSettingsPasswordMinLength :: !Word -- ^ Minimum length of user password
, authSettingsPageSize :: !Word -- ^ Page size of methods for authorisation API
} deriving (Generic, Show)

$(deriveJSON (derivePrefix "authSettings") ''AuthSettings)

-- | Convert into auth plugin configuration
makeAuthConfig :: ConnectionPool -> AuthSettings -> AuthConfig 
makeAuthConfig pool AuthSettings{..} = (defaultAuthConfig pool) {
    defaultExpire = fromIntegral authSettingsExpire
  , maximumExpire = fromIntegral <$> authSettingsMaximumExpire
  , restoreExpire = fromIntegral authSettingsRestoreExpire
  , passwordsStrength = authSettingsPasswordsStrength
  , passwordValidator = validatePassword authSettingsPasswordMinLength
  , defaultPageSize = authSettingsPageSize
  }

-- | Validate password for user, don't accept too small passwords
validatePassword :: Word -> Text -> Maybe Text 
validatePassword s t 
  | T.length t <= fromIntegral s = Just $ "Password should be greater than " <> showt s <> " symbols"
  | otherwise = Nothing