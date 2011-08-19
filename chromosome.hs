module Chromosome where

import Control.Monad.Random
import Data.Array
import Data.List
import Debug.Trace

import WeeklyTimes
import Class
import TA
import GraphConstructor

data SearchDirection = Forward | Backward deriving (Eq, Enum, Bounded, Ord, Show)
searchIncrement Forward = 1
searchIncrement Backward = (-1)

data GeneStatus = Resolved | Unresolved | Unresolvable deriving (Eq, Show)

data Gene = Gene { clss :: Class, taToAllocate :: Int, taDirection :: SearchDirection, nextGene :: Int, geneDirection :: SearchDirection, status :: GeneStatus }

instance Show Gene where
  show g = "Gene; class: " ++ (show (Class.name (clss g))) ++ "ta: " ++ (show (taToAllocate g)) ++ "; d: " ++ (show (taDirection g)) ++ "; next: " ++ (show (nextGene g)) ++ "; status: " ++ (show (status g)++"\n")

type Chromosome = Array Int Gene

data Generation = Generation { tas :: Array Int TA, chromosomes :: [Chromosome], resolved :: Bool } deriving Show

resolvedPairs :: Array Int TA -> Chromosome -> [PossiblePairing]
resolvedPairs tas chr = map pairFromGene (filter (\g -> (status g) == Resolved) (elems chr))
  where
    pairFromGene g = ((clss g),(tas ! (taToAllocate g)))

initialGeneration :: (RandomGen g) => Int -> [TA] -> [Class] -> Rand g Generation
initialGeneration n tas classes = do
  r <- rawGeneration
  return $ Generation (Chromosome.tas r) (map (fixSelfReferences 0) (chromosomes r)) (resolved r)
  
  where
    fixSelfReferences x c | x == (snd (bounds c))+1 = c
                          | otherwise = fixSelfReferences (x+1) (c // [(x,fixedGene)])
      where
        fixedGene = Gene (clss unfixedGene) (taToAllocate unfixedGene) (taDirection unfixedGene) fixedNextGene (geneDirection unfixedGene) (status unfixedGene)
        unfixedGene = (c ! x)
        fixedNextGene = case (nextGene unfixedGene) of y | (y == x)  -> (snd (bounds c))
                                                         | otherwise -> y
    rawGeneration = do
      let tasArray = listArray (0,(length tas)-1) tas
      chrs <- sequence (replicate n randomChromosome)
      return $ Generation tasArray chrs False
        where
          randomChromosome :: (RandomGen g) => Rand g Chromosome
          randomChromosome = do
            genes <- sequence (map randomGene classes)
            return $ listArray (0,(length genes)-1) genes

          randomGene :: (RandomGen g) => Class -> Rand g Gene
          randomGene c = do
            taIndex <- getRandomR (0,(length tas)-1)
            geneIndex <- getRandomR (0,(length classes)-2)
            taDirectionIndex <- getRandomR (0,1)
            geneDirectionIndex <- getRandomR (0,1)
            return $ Gene c taIndex (toEnum taDirectionIndex) geneIndex (toEnum geneDirectionIndex) Unresolved
          


resolve :: Generation -> Generation
resolve (Generation tas chromosomes _) = Generation tas (map (resolveChromosome 0 0) chromosomes) True
  where
    resolveChromosome start x chromosome | x==(-1) = chromosome
                                         | otherwise = resolveChromosome start trueNextUnresolvedGeneIndex (chromosome // [(x,currentResolvedGene)])
      where
        currentResolvedGene = Gene (clss currentUnresolvedGene) nextAllowedTAIndex (taDirection currentUnresolvedGene) nextUnresolvedGeneIndex (geneDirection currentUnresolvedGene) newStatus
        currentUnresolvedGene = chromosome ! x
        
        -- next two functions equal the given value if no allowed index is found
        nextAllowedTAIndex = iter (taToAllocate currentUnresolvedGene) False
          where
            iter index wrapped | index > snd (bounds tas) = iter 0 True
                               | index < 0 = iter (snd (bounds tas)) True
                               | index == (taToAllocate currentUnresolvedGene) && wrapped = index
                               | (taCanTeachClass (resolvedPairs tas chromosome) (clss currentUnresolvedGene) (tas ! index)) = index
                               | otherwise = iter (index+(searchIncrement (taDirection currentUnresolvedGene))) wrapped
        nextUnresolvedGeneIndex = iter (nextGene currentUnresolvedGene) False
          where
            iter index wrapped | index > snd (bounds chromosome) = iter 0 True
                               | index < 0 = iter (snd (bounds chromosome)) True
                               | index == (nextGene currentUnresolvedGene) && wrapped = index
                               | status (chromosome ! index) == Unresolved = index
                               | otherwise = iter (index+(searchIncrement (geneDirection currentUnresolvedGene))) wrapped
                                     
        -- equals -1 if no allowed index is found
        trueNextUnresolvedGeneIndex = case nextUnresolvedGeneIndex of x | x == (nextGene currentUnresolvedGene) && (status (chromosome ! (nextGene currentUnresolvedGene))) /= Unresolved -> (-1)
                                                                        | otherwise  -> nextUnresolvedGeneIndex
       
        nextTACanTeachClass = taCanTeachClass (resolvedPairs tas chromosome) (clss currentUnresolvedGene) (tas ! nextAllowedTAIndex)
        newStatus = case nextTACanTeachClass of True  -> Resolved
                                                False -> Unresolvable
                                                
fitness :: Chromosome -> Int
fitness chr = length (filter (\g -> (status g) == Resolved) (elems chr))

select :: Double -> Generation -> Generation
select percent (Generation tas chromosomes resolved) | percent <= 1.0 = Generation tas (drop (floor ((1.0-percent) * (fromIntegral (length chromosomes)))) (sortBy compareFitness chromosomes)) resolved
                                                     | otherwise = error "percent must be <= 1"
  where
    compareFitness c1 c2 = compare (fitness c1) (fitness c2)

nextGeneration :: (RandomGen g) => Int -> Double -> Double -> Generation -> Rand g Generation
nextGeneration size copyRate mutationRate oldGeneration = do
  chrs <- sequence (replicate size chromosomeSynthesizedFromOld)
  return $ Generation (tas oldGeneration) chrs False
  
  where
    chromosomeSynthesizedFromOld = do
      methodDie <- getRandomR (0.0,1.0)
      chr <- case methodDie of x | x <= copyRate -> randomChromosomeFromOld
                                 | otherwise     -> randomCrossoverFromOld
      mutateDie <- getRandomR (0.0,1.0)
      maybeMutatedChr <- case mutateDie of x | x <= mutationRate -> mutate oldGeneration chr
                                             | otherwise         -> return chr
      return $ maybeMutatedChr
        
    randomChromosomeFromOld = do
      index <- getRandomR (0,(length (chromosomes oldGeneration))-1)
      let chr = (chromosomes oldGeneration) !! index
      return $ listArray (bounds chr) $ unresolve $ elems chr
      
    randomCrossoverFromOld = do
      c1 <- randomChromosomeFromOld
      c2 <- randomChromosomeFromOld
      randomCrossover c1 c2
      
selectAndReproduce :: (RandomGen g) => Double -> Double -> Double -> Generation -> Rand g Generation
selectAndReproduce selectPercent copyRate mutationRate oldGeneration = 
  nextGeneration (length (chromosomes oldGeneration)) copyRate mutationRate (select selectPercent oldGeneration)
      
unresolve :: [Gene] -> [Gene]
unresolve genes = map unresolveGene genes
  where
    unresolveGene (Gene c t td ng gd _) = Gene c t td ng gd Unresolved

randomCrossover :: (RandomGen g) => Chromosome -> Chromosome -> Rand g Chromosome
randomCrossover c1 c2 = do
  numFromFirst <- getRandomR (0,clength)
  return $ listArray (0,clength) $ unresolve $ combinedElems numFromFirst
  where
    combinedElems numFromFirst = (take numFromFirst c1Elems) ++ (drop (numFromFirst) c2Elems)
    c1Elems = elems c1
    c2Elems = elems c2
    clength = snd (bounds c1)
    
mutate :: (RandomGen g) => Generation -> Chromosome -> Rand g Chromosome
mutate generation chromosome = do
  randomGeneIndex <- getRandomR (0,snd (bounds chromosome))
  let gene = chromosome ! randomGeneIndex
  randomTAIndex <- getRandomR (0,snd (bounds (tas generation)))
  taDirectionIndex <- getRandomR (0,1)
  geneDirectionIndex <- getRandomR (0,1)
  randomNextGeneIndex <- getRandomR (0,(snd (bounds chromosome))-1)
  let fixedNextGeneIndex = case randomNextGeneIndex of x | x == randomGeneIndex -> snd (bounds chromosome)
                                                         | otherwise -> randomNextGeneIndex
  let newgene = Gene (clss gene) randomTAIndex (toEnum taDirectionIndex) fixedNextGeneIndex (toEnum geneDirectionIndex) Unresolved
  return $ listArray (0,snd (bounds chromosome)) $ unresolve $ elems $ chromosome // [(randomGeneIndex,newgene)]
  
