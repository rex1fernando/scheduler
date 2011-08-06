{-# OPTIONS_GHC -XFlexibleInstances -XUndecidableInstances -XIncoherentInstances #-}

module Class where

import Data.Char
import WeeklyTimes
import qualified TA

data Classification = Recitation | Lab deriving (Show, Eq)

data Class = Class { name :: String, section :: String, classification :: Classification, meetingTimes :: [WeeklyTimeInterval] } deriving Show

instance Eq Class where
  (==) a b = (name a) == (name b)

instance Ord Class where
  (<=) a b = (map toLower (name a)) <= (map toLower (name b))

type PossiblePairing = (Class,TA.TA)

classesForTA :: TA.TA -> [PossiblePairing] -> [Class]
classesForTA ta pairings = map fst $ filter pairingHasTA pairings
  where
    pairingHasTA (c,t) = t == ta

busyTimesWithPairings :: TA.TA -> [PossiblePairing] -> [WeeklyTimeInterval]
busyTimesWithPairings ta existingPairs = (concat $ map meetingTimes $ classesForTA ta existingPairs) ++ (TA.busyTimes ta)

taHasTimeOpenWithPairings :: TA.TA -> [PossiblePairing] -> WeeklyTimeInterval -> Bool
taHasTimeOpenWithPairings ta existingPairs time = and $ map (not . intersecting time) (busyTimesWithPairings ta existingPairs)
