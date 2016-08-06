module Main where

import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp    (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Options.Applicative 

import Profile.Live.Server.API
import Profile.Live.Server.Application
import Profile.Live.Server.Config 
import Profile.Live.Server.Config.Auth
import Profile.Live.Server.Models 
import Profile.Live.Server.Monad

-- | Argument line options
data Options = Options {
  -- | Path to config, if not set, compile-time config will be used
  configPath :: Maybe FilePath  
}

-- | Parser of argument line options
options :: Parser Options 
options = Options 
  <$> (optional . strOption) (
       long "config"
    <> metavar "CONFIG"
    <> help "Path to configuration file"
    )

-- | Run sever with options
server :: Options -> IO ()
server Options{..} = do 
  config <- loadConfig configPath
  astate <- initAppState config
  
  let pool = appPool astate 
  let strength = authSettingsPasswordsStrength $ configAuth config
  runSqlPool (doMigrations strength) pool 
  generateJavaScript $ configStatic config <> "/api.js"
  generateSwagger $ configStatic config <> "/swagger.json"

  let port = fromIntegral $ configPort config
  let app = liveProfileApp astate
  let logger = setLogger $ configEnvironment config
  run port . logger . simpleCors $ app

main :: IO ()
main = execParser opts >>= server
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Runs live profile server for Haskell programs"
     <> header "live-profile-server - a live server for profiling Haskell programs" )