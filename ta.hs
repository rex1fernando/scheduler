module TA where
import WeeklyTimes

data Classification = HalfTime | QuarterTime deriving (Show)

data TA = TA {name :: String, classification :: Classification, busytimes :: BusyTimes} deriving (Show)

type BusyTimes = [WeeklyTimeInterval]