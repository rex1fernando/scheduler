module CSVReader where

import Text.CSV
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Data.String.Utils
import Data.Char
import qualified Class
import WeeklyTimes
import qualified TA

readClassesFromFile :: FilePath -> IO (Either ParseError [Class.Class])
readClassesFromFile fp = (do rs <- parseCSVFromFile fp
                             return (case rs of Left err -> Left err
                                                Right rs -> eitherFromList $ map recordToClass rs))
  where
    recordToClass :: Record -> Either ParseError Class.Class
    recordToClass r = do
      cl <- (parse classClassification "" (head (tail (tail r))))
      mt <- eitherFromList $ map (parse timeInterval "") (tail (tail (tail r)))
      --cl <- return Class.Lab
      return Class.Class { Class.name = (head r), 
                           Class.section = (head (tail r)),
                           Class.classification = cl,
                           Class.meetingTimes = mt
                           --Class.meetingTimes = (WeeklyTimeInterval (WeeklyTime Monday 0 0) (WeeklyTime Monday 0 0)):[]
                         }
        
readTAsFromFile :: FilePath -> IO (Either ParseError [TA.TA])
readTAsFromFile fp = (do rs <- parseCSVFromFile fp
                         return (case rs of Left err -> Left err
                                            Right rs -> eitherFromList $ map recordToTA rs))
  where
    recordToTA :: Record -> Either ParseError TA.TA
    recordToTA r = do
      cl <- (parse taClassification "" (head (tail r)))
      bt <- eitherFromList $ map (parse timeInterval "") (tail (tail r))
      --cl <- return Class.Lab
      return TA.TA { TA.name = (head r), 
                     TA.classification = cl,
                     TA.busyTimes = bt
                                    --Class.meetingTimes = (WeeklyTimeInterval (WeeklyTime Monday 0 0) (WeeklyTime Monday 0 0)):[]
                   }
        
eitherFromList :: [Either a b] -> Either a [b]        
eitherFromList l = iter [] l
  where
    iter :: [b] -> [Either a b] -> (Either a [b])
    iter b [] = Right (reverse b)
    iter b ((Right d):ds) = (iter (d:b) ds)
    iter b ((Left d):ds) = Left d

-- Parsers

--weeklyTimeInterval = do
--  whitespace
  
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x
    
classClassification :: Parser Class.Classification
classClassification = (do char 'R' <|> char 'r'
                          return Class.Recitation)
                      <|>
                      (do char 'L' <|> char 'l'
                          return Class.Lab)

taClassification :: Parser TA.Classification
taClassification = try (do string "0.5"
                           return TA.HalfTime)
                   <|>
                   (do string "0.25"
                       return TA.QuarterTime)

timeInterval :: Parser WeeklyTimeInterval
timeInterval = do
  spaces
  w <- weekday
  spaces
  startTime <- time
  spaces
  char '-'
  spaces
  endTime <- time
  start <- maybeParser (properWeeklyTime w (fst startTime) (snd startTime)) "Improperly formatted time"
  end <- maybeParser (properWeeklyTime w (fst endTime) (snd endTime)) "Improperly formatted time"
  maybeParser (properWeeklyTimeInterval start end) "start time cannot be greater than end time"
         
maybeParser :: Maybe a -> String -> Parser a
maybeParser (Just a) errorMessage = return a
maybeParser Nothing  errorMessage = fail errorMessage
  
weekday :: Parser WeekDay
weekday = try (do {char 'U' <|> char 'u'; return Sunday})
          <|> try (do {char 'M' <|> char 'm'; return Monday})
          <|> try (do {char 'T' <|> char 't'; return Tuesday})
          <|> try (do {char 'W' <|> char 'w'; return Wednesday})
          <|> try (do {char 'R' <|> char 'r'; return Thursday})
          <|> try (do {char 'F' <|> char 'f'; return Friday})
          <|> try (do {char 'U' <|> char 'u'; return Saturday})
          
apHourToMilitary 12 'a' = 0
apHourToMilitary hour 'a' = hour
apHourToMilitary 12 'p'   = 12
apHourToMilitary hour 'p' = hour+12
          
time :: Parser (Int,Int)
time = do
  h <- many1 digit
  m <- do { char ':'; count 2 digit } <|> (return "0")
  spaces
  ap <- char 'a' <|> char 'p'
  let hi = apHourToMilitary (read h) ap
  return (hi,(read m))
  
convert n []     = n
convert n (d:ds) = convert (10*n + digitToInt d) ds