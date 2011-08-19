module GraphConstructor where

import CSVReader
import Class
import TA
import WeeklyTimes

possiblePairings :: [Class] -> [TA] -> [PossiblePairing] -> [PossiblePairing]
possiblePairings classes tas existingPairs = concat $ map pairsWithClass classes
  where
    pairsWithClass c = map (pairWithClass c) (filter (taCanTeachClass existingPairs c) tas)
    pairWithClass c ta = (c,ta)


taCanTeachClass :: [PossiblePairing] -> Class -> TA -> Bool
taCanTeachClass existingPairs c ta = alreadyTeachingSection && teachingQuotaWillBeObserved && (and $ map (taHasTimeOpenWithPairings ta existingPairs) $ meetingTimes c)
  where
    -- if no existing pairs, then TA is "already" teaching any section
    alreadyTeachingSection = iter existingPairs
      where
        iter [] = True
        iter ((c1,t1):rep) | (t1 == ta) && ((section c1) /= (section c)) = False
                           | otherwise = iter rep
    
    teachingQuotaWillBeObserved = case (TA.classification ta) of x | x == HalfTime    -> iter 2 existingPairs
                                                                   | x == QuarterTime -> iter 1 existingPairs
      where
        iter 0 _ = False
        iter _ [] = True
        iter x ((c1,t1):ps) | (t1 == ta) && ((Class.classification c1) == (Class.classification c)) = iter (x-1) ps
                            | otherwise = iter x ps