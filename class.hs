module Class where

import WeeklyTimes
import qualified TA

data Classification = Recitation | Lab

instance Read Classification where
  read "R" ++ s = (Recitation,s)
  read "L" ++ s = (Lab,s

data Class = Class { name :: String, classification :: Classification, meetingTimes :: [WeeklyTimeInterval] }

type PossiblePairings = [(Class,TA.TA)]