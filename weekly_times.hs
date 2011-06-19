module WeeklyTimes where

data WeekDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday deriving (Show, Eq, Enum, Bounded, Ord)

data WeeklyTime = WeeklyTime WeekDay Int Int deriving (Eq, Show)

minutesPastSundayMidnight :: WeeklyTime -> Int
minutesPastSundayMidnight (WeeklyTime d h m) = (fromEnum d)*1440+h*60+m

instance Ord WeeklyTime where
  compare t1 t2 = compare (minutesPastSundayMidnight t1) (minutesPastSundayMidnight t2)

data WeeklyTimeInterval = WeeklyTimeInterval WeeklyTime WeeklyTime deriving (Show)

instance Read WeeklyTimeInterval where
  read s = do
    whitespace
    

properWeeklyTimeInterval :: WeeklyTime -> WeeklyTime -> Maybe WeeklyTimeInterval
properWeeklyTimeInterval t1 t2 | t1 < t2 = Just (WeeklyTimeInterval t1 t2)
                               | t1 >= t2 = Nothing

intersecting :: WeeklyTimeInterval -> WeeklyTimeInterval -> Bool
intersecting (WeeklyTimeInterval firstStart firstEnd) (WeeklyTimeInterval secondStart secondEnd)
  = (firstEnd > secondStart) && (secondEnd > firstStart)