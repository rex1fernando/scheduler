module Class where

import WeeklyTimes
import qualified TA

data Classification = Recitation | Lab deriving (Show, Eq)

data Class = Class { name :: String, classification :: Classification, meetingTimes :: [WeeklyTimeInterval] } deriving Show

type PossiblePairings = [(Class,TA.TA)]