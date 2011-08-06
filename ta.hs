{-# OPTIONS_GHC -XFlexibleInstances -XUndecidableInstances -XIncoherentInstances #-}

module TA where

import WeeklyTimes
import Data.Char

data Classification = HalfTime | QuarterTime deriving (Show)

data TA = TA {name :: String, classification :: Classification, busyTimes :: BusyTimes} deriving (Show)

instance Eq TA where
  (==) a b = (name a) == (name b)

instance Ord TA where
  (<=) a b = (map toLower (name a)) <= (map toLower (name b))

type BusyTimes = [WeeklyTimeInterval]

taHasTimeOpen ta time = and $ map (not . intersecting time) (busyTimes ta)

