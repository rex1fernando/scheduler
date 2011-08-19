module MemoryRing (MemoryRing, SearchDirection(Forward, Backward)) where

import Data.Array

data SearchDirection = Forward | Backward deriving (Eq, Enum, Bounded, Ord, Show)

type MemoryRing a = Array Int (MemoryRing

data Node = Node { element :: a, direction :: SearchDirection, next :: Int }

