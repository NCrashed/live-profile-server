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
  , importLocal
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Maybe
import Data.Monoid 
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
import System.Directory
import System.Directory.Tree
import System.FilePath

import qualified Data.ByteString.Lazy as B

import Profile.Live.Server.API.Connection 
import Profile.Live.Server.Application.EventLog 
import Profile.Live.Server.Application.Session
import Profile.Live.Server.Config
import Profile.Live.Server.Monad 

connectionServer :: ServerT ConnectionAPI App 
connectionServer = restServer (Proxy :: Proxy '[ 'GET, 'POST, 'PUT, 'PATCH, 'DELETE]) 
  (Proxy :: Proxy Connection) (Proxy :: Proxy "connection")
  (Proxy :: Proxy App)
  :<|> listConn
  :<|> importLocalMethod

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

-- | Import all eventlog files from server special folder
importLocalMethod :: Id Connection 
  -> MToken' '["write-connection"]
  -> App Unit
importLocalMethod i token = do 
  guardAuthToken token 
  importLocal i
  return Unit

-- | Import all eventlog files from server special folder
importLocal :: Id Connection -> App ()
importLocal i = do 
  ImportConfig{..} <- getsConfig configImport
  importLog $ "Start local import from " <> showl importConfigFolder
  (_ :/ dTree) <- liftIO $ readDirectoryWithL B.readFile importConfigFolder
  case dTree of 
    Failed{..} -> importLog $ "Failed to open folder, reason: " <> showl err 
    File{..} -> importLog "Failed, not a directory"
    Dir{..} -> forM_ (flatFiles contents) $ \(nm, bs) ->
      processFile nm bs importConfigFolder importConfigSuccess
      `catchAll`   
      (\e -> do
        importLog $ "Failed: " <> showl e 
        moveToFailure nm importConfigFolder importConfigFailure )
  where 
  importLog = appLog . ("Import: " <>) 

  processFile name bs f fSucc = do 
    importLog $ "Importing file " <> showl name
    either fail (void . runDB . importFakeSession i) $ parseEventLog bs
    moveToSuccess name f fSucc

  moveToSuccess name f fSucc = liftIO $ renameFile (f </> name) (fSucc </> name)
  moveToFailure name f fFail = liftIO $ renameFile (f </> name) (fFail </> name)

  flatFiles nodes = catMaybes $ extractFile <$> nodes
    where 
    extractFile node = case node of 
      File{..} -> Just (name, file)
      _ -> Nothing
