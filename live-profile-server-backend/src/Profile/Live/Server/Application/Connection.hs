{-|
Module      : Profile.Live.Server.Application.Connection
Description : Implementation of Connection API
Copyright   : (c) Anton Gushcha, 2016
License     : BSD3
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable
-}
module Profile.Live.Server.Application.Connection(
    connectionServer
  ) where

import Control.Monad
import Data.Aeson.WithField
import Data.Maybe
import Data.Proxy 
import Database.Persist
import Servant.API 
import Servant.API.Auth.Token
import Servant.API.Auth.Token.Pagination
import Servant.API.REST.Derive
import Servant.API.REST.Derive.Server
import Servant.API.REST.Derive.Server.Vinyl
import Servant.Server 
import Servant.Server.Auth.Token

import Profile.Live.Server.API.Connection 
import Profile.Live.Server.Monad 

connectionServer :: ServerT ConnectionAPI App 
connectionServer = restServer (Proxy :: Proxy '[ 'GET, 'POST, 'PUT, 'PATCH, 'DELETE]) 
  (Proxy :: Proxy Connection) (Proxy :: Proxy "connection")
  (Proxy :: Proxy App)
  :<|> listConn

listConn :: Maybe Page 
  -> Maybe PageSize
  -> MToken' '["read-connection"]
  -> App (PagedList (Id Connection) Connection)
listConn mp msize token = do 
  guardAuthToken token 
  pagination mp msize $ \page size -> do 
    (es, total) <- runDB $ (,)
      <$> (do
        (is :: [Key Connection]) <- selectKeysList [] [OffsetBy (fromIntegral $ page * size), LimitTo (fromIntegral size)]
        forM is $ (\i -> fmap (WithField i) <$> readResource i) . unVKey)
      <*> count ([] :: [Filter Connection])
    return PagedList {
        pagedListItems = catMaybes es
      , pagedListPages = ceiling $ (fromIntegral total :: Double) / fromIntegral size
      }