module CSVReader where

import Text.CSV
import Data.String.Utils

readClassesFromFile :: FilePath -> IO ([Class])
readClassesFromFile fp = map recordToClass (parseCSVFromFile fp)
  where
    recordToClass :: Record -> Class
    recordToClass r = Class { name = (head r), 
                              classification = fst (read (head (tail r))),
                              meetingTimes = map fst (map read (head (tail (tail r))))
                            }


-- Parsers

weeklyTimeInterval = do
  whitespace
  
f