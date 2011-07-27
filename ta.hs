module TA where
import WeeklyTimes

data Classification = HalfTime | QuarterTime deriving (Show)

data TA = TA {name :: String, classification :: Classification, busyTimes :: BusyTimes} deriving (Show)

instance Eq TA where
  (==) a b = (name a) == (name b)

type BusyTimes = [WeeklyTimeInterval]

taHasTimeOpen ta time = and $ map (not . intersecting time) (busyTimes ta)

