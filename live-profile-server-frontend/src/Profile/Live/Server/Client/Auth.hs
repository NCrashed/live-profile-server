{-# OPTIONS_GHC -fno-warn-orphans #-}
module Profile.Live.Server.Client.Auth(
    authSignin
  , authTouch
  , authInfo
  , authSignout
  , authSignup
  , authUsersInfo
  , authUserInfo
  , authUserPatch
  , authUserPut
  , authUserDelete
  , authRestore
  , authGroupGet
  , authGroupPost
  , authGroupPut
  , authGroupPatch
  , authGroupDelete
  , authGroups
  , SimpleToken
  ) where 

import Control.Monad.Trans.Either 
import Data.Aeson.Unit 
import Data.Aeson.WithField
import Servant.API
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.Client 

import GHCJS.Marshal

import Profile.Live.Server.Client.Async 

-- | How to get a token, expire of 'Nothing' means 
-- some default value (server config)
authSignin :: Maybe Login -- ^ Login query parameter
  -> Maybe Password-- ^ Password query parameter
  -> Maybe Seconds -- ^ Expire query parameter, how many seconds the token is valid
  -> EitherT ServantError IO (OnlyField "token" SimpleToken) 

-- | Client cat expand the token lifetime 
authTouch :: Maybe Seconds -- ^ Expire query parameter, how many seconds the token should be valid by now. 'Nothing' means default value defined in server config.
  -> MToken' '[] -- ^ Authorisation header with token 
  -> EitherT ServantError IO Unit

-- | Close session, after call of the method the
-- token in header is not valid.
authInfo :: MToken' '[] -- ^ Authorisation header with token 
  -> EitherT ServantError IO RespUserInfo

-- | Close session, after call of the method the
-- token in header is not valid.
authSignout :: MToken' '[] -- ^ Authorisation header with token 
  -> EitherT ServantError IO Unit

-- | Creation of new user, requires 'registerPerm' for token
authSignup :: ReqRegister -- ^ Registration info
  -> MToken' '["auth-register"] -- ^ Authorisation header with token 
  -> EitherT ServantError IO (OnlyField "user" UserId)

-- | Getting list of all users, requires 'authInfoPerm' for token
authUsersInfo :: Maybe Page -- ^ Page num parameter
  -> Maybe PageSize -- ^ Page size parameter
  -> MToken' '["auth-info"] -- ^ Authorisation header with token 
  -> EitherT ServantError IO RespUsersInfo

-- | Getting info about user, requires 'authInfoPerm' for token
authUserInfo :: UserId -- ^ User id 
  -> MToken' '["auth-info"] -- ^ Authorisation header with token 
  -> EitherT ServantError IO RespUserInfo 

-- | Updating login/email/password, requires 'authUpdatePerm' for token
authUserPatch :: UserId -- ^ User id 
  -> PatchUser -- ^ JSON with fields for patching
  -> MToken' '["auth-update"] -- ^ Authorisation header with token 
  -> EitherT ServantError IO Unit

-- | Replace user with the user in the body
authUserPut :: UserId -- ^ User id 
  -> ReqRegister -- ^ New user
  -> MToken' '["auth-update"] -- ^ Authorisation header with token 
  -> EitherT ServantError IO Unit

-- | Delete user from DB, requires 'authDeletePerm' and will cause cascade
-- deletion, that is your usually want
authUserDelete :: UserId -- ^ User id 
  -> MToken' '["auth-delete"] -- ^ Authorisation header with token 
  -> EitherT ServantError IO Unit

-- | Generate new password for user. There is two phases, first, the method
-- is called without 'code' parameter. The system sends email with a restore code
-- to email. After that a call of the method with the code is needed to 
-- change password. Need configured SMTP server.
authRestore :: UserId
  -> Maybe RestoreCode
  -> Maybe Password 
  -> EitherT ServantError IO Unit

-- | Getting info about user group, requires 'authInfoPerm' for token
authGroupGet :: UserGroupId 
  -> MToken' '["auth-info"]
  -> EitherT ServantError IO UserGroup

-- | Inserting new user group, requires 'authUpdatePerm' for token
authGroupPost :: UserGroup 
  -> MToken' '["auth-update"]
  -> EitherT ServantError IO (OnlyId UserGroupId)

-- | Replace info about given user group, requires 'authUpdatePerm' for token
authGroupPut :: UserGroupId 
  -> UserGroup 
  -> MToken' '["auth-update"]
  -> EitherT ServantError IO Unit

-- | Replace info about given user group, requires 'authUpdatePerm' for token
authGroupPatch :: UserGroupId 
  -> PatchUserGroup 
  -> MToken' '["auth-update"]
  -> EitherT ServantError IO Unit

-- | Delete all info about given user group, requires 'authDeletePerm' for token
authGroupDelete :: UserGroupId
  -> MToken' '["auth-delete"]
  -> EitherT ServantError IO Unit

-- | Get list of user groups, requires 'authInfoPerm' for token 
authGroups :: Maybe Page 
  -> Maybe PageSize 
  -> MToken' '["auth-info"]
  -> EitherT ServantError IO (PagedList UserGroupId UserGroup) 

(      authSignin
  :<|> authTouch
  :<|> authInfo
  :<|> authSignout
  :<|> authSignup
  :<|> authUsersInfo
  :<|> authUserInfo
  :<|> authUserPatch
  :<|> authUserPut
  :<|> authUserDelete
  :<|> authRestore
  :<|> authGroupGet
  :<|> authGroupPost
  :<|> authGroupPut
  :<|> authGroupPatch
  :<|> authGroupDelete
  :<|> authGroups
    ) = client authAPI host
  where host = Nothing

instance ToJSVal ReqRegister where 
  toJSVal = toJSVal_aeson
instance ToJSVal PatchUser where 
  toJSVal = toJSVal_aeson
instance ToJSVal UserGroup where 
  toJSVal = toJSVal_aeson
instance ToJSVal PatchUserGroup where 
  toJSVal = toJSVal_aeson
instance FromJSVal RespUsersInfo where 
  fromJSVal = fromJSVal_aeson
instance FromJSVal RespUserInfo where 
  fromJSVal = fromJSVal_aeson
instance FromJSVal UserGroup where 
  fromJSVal = fromJSVal_aeson