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

import Servant.Server 
--import Servant.Server.Auth.Token 

import Profile.Live.Server.API.Upload 
--import Profile.Live.Server.Application.Upload.Model 
--import Profile.Live.Server.Config
--import Profile.Live.Server.Error
import Profile.Live.Server.Monad 
--import Profile.Live.Server.Utils

-- | Implementation of 'UploadAPI'
uploadServer :: ServerT UploadAPI App 
uploadServer = error "unimplemented UploadAPI"