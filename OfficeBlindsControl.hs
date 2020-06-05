module Main where

import           System.IO
import           Data.Char
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

data Command = Down | Up | Stop deriving Show
data Step = Step { blindId :: Int, command :: Command } deriving Show
type StepSet = (Int, [Step])

timescaleParser :: Parser Int
timescaleParser = do
  manyTill anyChar (try (string "timescale="))
  timescale <- read <$> many1 digit
  skipMany1 endOfLine
  return timescale


stepParser :: Parser Step
stepParser = do
  skipMany1 space
  blindId <- read <$> many1 digit
  skipMany1 space
  command <- string "down" <|> string "up" <|> string "stop"
  skipMany1 endOfLine
  return Step
    { blindId = blindId
    , command = case command of
                  "down" -> Down
                  "up"   -> Up
                  "stop" -> Stop
    }


intervalParser :: Parser StepSet
intervalParser = do
  time  <- read <$> many1 digit
  steps <- many1 stepParser
  return (time, steps)


-- (500, [(0, [Step { blindId = 0, command = Down }])])
mainParser :: Parser (Int, [StepSet])
mainParser = do
  timescale <- timescaleParser
  intervals <- many1 intervalParser
  return (timescale, intervals)


main = do
  rawInstructionsHandle <- openFile "./blinds-instructions.txt" ReadMode
  rawInstructions       <- hGetContents rawInstructionsHandle

  case parse mainParser "" (map toLower rawInstructions) of
    Left  e -> print e
    Right i -> print i
