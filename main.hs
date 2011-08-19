import Control.Monad.Random
import Data.Array
import Data.List

import CSVReader
import Chromosome
import GraphConstructor


main :: IO ()
main = do
  (Right tas) <- readTAsFromFile "tas.csv"
  (Right classes) <- readClassesFromFile "classes.csv"
  g <- evalRandIO $ initialGeneration 100 tas classes
  reproduceMany 1000 g
  return ()
  
reproduceMany :: Int -> Generation -> IO ()
reproduceMany times gen = iter times times gen 
  where
    iter times remaining g = do
      let rg = resolve g
      let fa = map fitness (chromosomes rg)
      --  print fa
      print $ mean fa
      print $ maximum fa
      case remaining of x | x > 0         -> do { sg <- evalRandIO $ selectAndReproduce (0.5-(0.45*((fromIntegral (times-remaining))::Double)/((fromIntegral times)::Double))) 0.48 0.1 rg; iter times (remaining-1) sg }
    --                      | otherwise     -> do { print (chromosomes rg); print (tas rg) }
                          | otherwise     -> do { print (best (chromosomes rg)); print (tas rg) }
                                 
mean :: [Int] -> Double
mean a = ((fromIntegral $ sum a)::Double)/((fromIntegral (length a))::Double)

best :: [Chromosome] -> Chromosome
best chromosomes = iter chromosomes (head chromosomes) 
  where
    iter []     bestSoFar = bestSoFar
    iter (c:cs) bestSoFar | (fitness c) > (fitness bestSoFar) = iter cs c
                          | otherwise = iter cs bestSoFar
                                        
test :: IO ()
test = do
  (Right tas) <- readTAsFromFile "tas.csv"
  (Right classes) <- readClassesFromFile "classes.csv"
  let existingPairs = [(classes !! 5,tas !! 3),(classes !! 6,tas !! 3),(classes !! 9,tas !! 3),(classes !! 12,tas !! 3),(classes !! 13,tas !! 3)]
  print $ taCanTeachClass existingPairs (classes !! 14) (tas !! 3)