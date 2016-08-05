{-|
Module      : Profile.Live.Server.Application
Description : Creation of WAI application
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application(

  ) where

import Control.Monad.Except 
import Control.Monad.Reader 
import Servant 
import Servant.API 

import Profile.Live.Server.API
import Profile.Live.Server.Config 
import Profile.Live.Server.Monad 

-- | Handlers for core API that the server implements
coreServer :: AppState -> Server CoreLiveProfileAPI
coreServer app = enter (convertApp app) (return ()) -- temp, here handlers are added

-- | This function converts our 'App' monad into the @ExceptT ServantErr
-- IO@ monad that Servant's 'enter' function needs in order to run the
-- application. The ':~>' type is a natural transformation, or, in
-- non-category theory terms, a function that converts two type
-- constructors without looking at the values in the types.
convertApp :: AppState -> App :~> ExceptT ServantErr IO
convertApp app = Nat (flip runReaderT app . runApp)