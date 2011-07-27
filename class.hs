module Class where

import WeeklyTimes
import qualified TA

data Classification = Recitation | Lab deriving (Show, Eq)

data Class = Class { name :: String, section :: String, classification :: Classification, meetingTimes :: [WeeklyTimeInterval] } deriving Show

type PossiblePairing = (Class,TA.TA)

classesForTA :: TA.TA -> [PossiblePairing] -> [Class]
classesForTA ta pairings = map fst $ filter pairingHasTA pairings
  where
    pairingHasTA (c,t) = t == ta

busyTimesWithPairings :: TA.TA -> [PossiblePairing] -> [WeeklyTimeInterval]
busyTimesWithPairings ta existingPairs = (concat $ map meetingTimes $ classesForTA ta existingPairs) ++ (TA.busyTimes ta)

taHasTimeOpenWithPairings :: TA.TA -> [PossiblePairing] -> WeeklyTimeInterval -> Bool
taHasTimeOpenWithPairings ta existingPairs time = and $ map (not . intersecting time) (busyTimesWithPairings ta existingPairs)
