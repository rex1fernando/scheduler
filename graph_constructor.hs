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
taCanTeachClass existingPairs c ta = alreadyTeachingSection c && (and $ map (taHasTimeOpenWithPairings ta existingPairs) $ meetingTimes c)
  where
    alreadyTeachingSection c = case existingPairs of [] -> True
                                                     (c1,t) : _ -> (section c1) == (section c)