module Main where 

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (Async, async, wait)
import GHC.Conc (labelThread)

import Profile.Live.Leech

-- import Control.Concurrent (threadDelay)
-- import Control.Exception (bracket)
-- import Debug.Trace 

opts :: LeechOptions 
opts = defaultLeechOptions {
    leechBufferSize = 100
  }

-- main :: IO ()
-- main = bracket (startLeech opts) (const stopLeech) $ const $ go 0
--   where 
--   go :: Int -> IO ()
--   go i = do 
--     traceEventIO $ "MyEvent" ++ show i
--     --putStrLn $ "MyEvent" ++ show i
--     threadDelay 1000
--     go (i+1)

async' :: String -> IO a -> IO (Async a)
async' label act = async $ do
  tid <- myThreadId
  labelThread tid label
  act

-- Intentionally slow fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

printFib :: Integer -> IO ()
printFib n = withLiveEventIO ("fib" ++ show n) $ print (fib n)

blips :: IO ()
blips = do
  putStrLn "BLIP"
  threadDelay 5000000
  putStrLn "BLIP"

main :: IO ()
main = do
  a1 <- async' "evens" $ mapM_ printFib [30, 32 .. 38]
  a2 <- async' "odds"  $ mapM_ printFib [31, 33 .. 39]
  threadDelay 5000000
  a3 <- async' "blips"  $ blips
  mapM_ wait [a1, a2, a3]