module Readers where

import Text.Read
import Text.ParserCombinators.Parsec
import qualified Class
import qualified WeeklyTimes



classificationParser :: Parser Class.Classification
classificationParser = (do char 'R' <|> char 'r'
                           return Class.Recitation)
                       <|>
                       (do char 'L' <|> char 'l'
                           return Class.Lab)
               
--instance (ParsecRead a) => (Read a) where
--  readsPrec _ = either (const []) id . parse parsecRead' "" where
--    parsecRead' = do b <- parsecRead
--                     rest <- getInput
--                     return [(b, rest)]
                     
