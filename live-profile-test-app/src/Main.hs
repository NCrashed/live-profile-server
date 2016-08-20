module Main where 

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Debug.Trace 

import Profile.Live.Leech

main :: IO ()
main = bracket (startLeech defaultLeechOptions) (const stopLeech) $ const $ go 0
  where 
  go :: Int -> IO ()
  go i = do 
    traceEventIO $ "MyEvent" ++ show i
    threadDelay 3000000
    go (i+1)