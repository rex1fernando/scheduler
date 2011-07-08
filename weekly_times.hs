module WeeklyTimes where

data WeekDay = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Eq, Enum, Bounded, Ord)

data WeeklyTime = WeeklyTime WeekDay Int Int deriving (Eq, Show)

properWeeklyTime :: WeekDay -> Int -> Int -> Maybe WeeklyTime
properWeeklyTime d h m | h >= 24 || m >= 60 || h < 0 || m < 0 = Nothing
                       | otherwise = Just (WeeklyTime d h m)

minutesPastSundayMidnight :: WeeklyTime -> Int
minutesPastSundayMidnight (WeeklyTime d h m) = (fromEnum d)*1440+h*60+m

instance Ord WeeklyTime where
  compare t1 t2 = compare (minutesPastSundayMidnight t1) (minutesPastSundayMidnight t2)

data WeeklyTimeInterval = WeeklyTimeInterval WeeklyTime WeeklyTime deriving (Show)
    

properWeeklyTimeInterval :: WeeklyTime -> WeeklyTime -> Maybe WeeklyTimeInterval
properWeeklyTimeInterval t1 t2 | t1 < t2 = Just (WeeklyTimeInterval t1 t2)
                               | t1 >= t2 = Nothing

intersecting :: WeeklyTimeInterval -> WeeklyTimeInterval -> Bool
intersecting (WeeklyTimeInterval firstStart firstEnd) (WeeklyTimeInterval secondStart secondEnd)
  = (firstEnd > secondStart) && (secondEnd > firstStart)