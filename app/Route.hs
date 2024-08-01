module Route where

import Control.Concurrent.MVar
import Data.IORef
import Node
import Prelude
import Data.Vector 


--data NodeList = Empty | 

data Route = Route 
    {
        time :: Float,
        routeLength :: Int, 
        route :: Vector Node, 
        nodeList :: Vector Int
    }

threadRange :: Int -> Int -> Int -> Int -> (Int, Int)
threadRange lower upper tIndex tAmount  = (lower + findLowerBound tIndex, lower + findLowerBound (tIndex + 1) - 1)
  where 
    findLowerBound 0 = 0        
    findLowerBound x | x <= (upper - lower) `mod` tAmount = threadSize * x + x        --(upper - lower) `mod` tAmount holds the amount of threads which will get one extra number in their range to validate.
                     | otherwise = threadSize * x + (upper - lower) `mod` tAmount
    threadSize = (upper - lower) `div` tAmount      --The standard amount of work for a thread.


optimiseRoute :: Int -> Vector Int -> IO Route
optimiseRoute lowFound nodes = 
    do lowValue <- newMVar lowFound 
       bestRoute <- newEmptyMVar (:: IO (MVar Route))
       --Roep threads aan, die elk een gedeelte afzoeken.
       forkThreads 8 (branchAndBound nodes lowValue bestRoute (Route {time = 0, routeLength = 0, route = generate 1179 (\index -> Node {id = index, prev = -1, next = -1}) }))
       readMVar bestRoute

branchAndBound :: Vector Int -> MVar Int -> MVar Route -> Route -> Int -> IO ()
branchAndBound nodes bestScore bestRoute currentRoute threadIndex = 
    do let (lower, upper) = threadRange 0 ((length nodes) - 1) threadIndex 8
        return 

outputBest :: IO Route -> IO ()
outputBest = undefined  

-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n work = do
  -- Fork the threads and create a list of the MVars which
  -- per thread tell whether the work has finished.
  finishVars <- mapM work' [0 .. n - 1]
  -- Wait on all MVars
  mapM_ takeMVar finishVars
  where
    work' :: Int -> IO (MVar ())
    work' index = do
      var <- newEmptyMVar
      _   <- forkOn index (work index >> putMVar var ())
      return var