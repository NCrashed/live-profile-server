module Main where 

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Debug.Trace 

import Profile.Live.Leech

opts :: LeechOptions 
opts = defaultLeechOptions {
    leechBufferSize = 100
  }

main :: IO ()
main = bracket (startLeech opts) (const stopLeech) $ const $ go 0
  where 
  go :: Int -> IO ()
  go i = do 
    traceEventIO $ "MyEvent" ++ show i
    --putStrLn $ "MyEvent" ++ show i
    threadDelay 1000
    go (i+1)