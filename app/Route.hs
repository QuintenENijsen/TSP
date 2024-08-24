module Route where

import Control.Concurrent.MVar
import Data.IORef
import Node
import Afstanden
import Prelude
import Data.Vector as V
import qualified Data.Vector.Mutable as MV


--data NodeList = Empty | 

data Route = Route 
    {
        time :: Float,
        routeLength :: Int, 
        route :: Vector Node, 
        lastNode :: Int
        --nodeList :: Vector Int,           deze zou gebruikt kunnen worden voor het random kunnen verkrijgen van een node. Is nu nog niet perse nodig.
    }

data City = City 
    {
        id :: Int,
        visited :: Bool
    }

afstandenTabel :: Vector (Vector Int)
afstandenTabel = undefined

ledigingsduurTabel :: Vector Float
ledigingsduurTabel = undefined

threadRange :: Int -> Int -> Int -> Int -> (Int, Int)
threadRange lower upper tIndex tAmount  = (lower + findLowerBound tIndex, lower + findLowerBound (tIndex + 1) - 1)
  where 
    findLowerBound 0 = 0        
    findLowerBound x | x <= (upper - lower) `mod` tAmount = threadSize * x + x        --(upper - lower) `mod` tAmount holds the amount of threads which will get one extra number in their range to validate.
                     | otherwise = threadSize * x + (upper - lower) `mod` tAmount
    threadSize = (upper - lower) `div` tAmount      --The standard amount of work for a thread.


optimiseRoute :: Int -> Vector Int -> IO ()
optimiseRoute lowFound nodes = 
    do bestScore <- newMVar lowFound 
       bestRoute <- newEmptyMVar :: IO (MVar Route)
       --Roep threads aan, die elk een gedeelte afzoeken.
       let cities = map (\x -> City {id = x, visited = False}) nodes
       forkThreads 8 (startSearch cities bestScore bestRoute (Route {time = 0, routeLength = 0, route = generate 1179 (\index -> Node {id = index, prev = -1, next = -1}) }))
       bestRoute <- readMVar bestRoute
       putStrLn $ show time (bestRoute)
       putStrLn $ show route (bestRoute)

startSearch :: Vector City -> MVar Int -> MVar Route -> Route -> Int -> IO ()
startSearch nodes bestScore bestRoute currentRoute threadIndex = 
    do  let cities =  map (\x -> City {id = x, visited = False}) nodes
            (lower, upper) = threadRange 0 ((length nodes) - 1) threadIndex 8
        V.imap (\index x -> if startCondition then (branchAndBound (V.modify (\v -> MV.write v index City {id = id (cities ! index), visited = True}) cities) bestScore bestRoute newRoute (cities ! index)) nodes)
            else return ()


startCondition :: Int -> Int -> Int -> Bool
startCondition val low high = val >= low && val <= high

branchAndBound :: Vector City -> MVar Int -> MVar Route -> Route -> City -> IO ()
branchAndBound cities bestScore bestRoute currentRoute city = 
    if (time route) > (readMVar bestScore)           --Dit is je bound.    
            then return ()  
            else $  
        do unless (visited city) $  
            do newRoute <- addCityToRoute city route    
            case (routeLength newRoute) of   
                    (length nodes) -> $ finalCheck bestScore bestRoute currentRoute 
                    _               -> V.imap (\index x -> branchAndBound (V.modify (\v -> MV.write v index City {id = id (cities ! index), visited = True}) cities) bestScore bestRoute newRoute (cities ! index))    

{- 
iMap :: (a -> Int -> b) -> [a] -> [b]
iMap _ [] = []
iMap fn xs = mapping fn 0 xs
    where 
        mapping :: (a -> Int -> b) -> Int -> [a] -> [b]
        mapping _ _ [] = []
        mapping fn index (x:xs) = (fn x index) : (mapping fn (index + 1) xs)
-}

addCityToRoute :: City -> Route -> Route
addCityToRoute (City {id = node}) (Route {time = time, routeLength = length, route = route, lastNode = last }) = 
    let newTime = time + (ledigingsduurTabel ! node) + ((afstandenTabel ! last) ! node)
        newRoute = V.modify (\v -> MV.write v node (Node {id = node, prev = last, next = -1})) route
    in Route {time = newTime, routeLength = length + 1, route = newRoute, lastNode = Node {id = node, prev = last, next = -1}}
        --Gebruik de laatste node info om te zoeken in de dubbele vector met alle afstanden, voeg dan deze nieuwe afstand toe aan de routelength
        --Verder ook nog de ledigingsduur eraan toevoegen. Hoog routeLength ook nog met 1 op.

finalCheck :: MVar Int -> MVar Route -> Route -> IO ()
finalCheck bestScore bestRoute currentRoute = 
    do currentBest <- takeMVar bestScore
                   if (routeLength route) < currentBest
                        then _ <- takeMVar bestRoute
                             putMVar bestRoute currentRoute
                        else putMVar bestScore currentBest

--Returned een string of file met de beste uitkomst.
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