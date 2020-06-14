{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           Data.Char
import           Text.Printf
import           Text.Parsec
import           Text.Parsec.String             ( Parser )
import           Network.Socket                 ( withSocketsDo )
import           Control.Concurrent
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS
import qualified Data.CaseInsensitive          as CI

data Command = Down | Up | Stop deriving Show
data Step = Step { blindId :: Int, command :: Command } deriving Show
--             (time, [Step])
type StepSet = (Int, [Step])
type Instructions = (Int, [StepSet])

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


-- (timescale, [StepSet])
-- (500, [(0, [Step { blindId = 0, command = Down }])])
mainParser :: Parser Instructions
mainParser = do
  timescale <- timescaleParser
  intervals <- many1 intervalParser
  return (timescale, intervals)

constructPackage :: Int -> String -> String
constructPackage =
  printf
    "{action:\"write\",address:%d,datatype:1,type:\"text\",update:false,value:\"%s\"}"

-- handleStepSet conn stepSet = do
--   WS.sendTextData conn ()

wsApp :: Instructions -> WS.ClientApp ()
wsApp instructions conn = do
  putStrLn "Connected!"

  WS.sendTextData conn (T.pack (constructPackage 17209 "0"))
  -- map () (snd instructions)

  WS.sendClose conn ("KTHXBYE" :: Text)
  putStrLn "Disconnected!"

main = do
  rawInstructionsHandle <- openFile "./OfficeBlindsControl1/blinds-instructions.txt" ReadMode
  rawInstructions       <- hGetContents rawInstructionsHandle

  case parse mainParser "" (map toLower rawInstructions) of
    Left  e -> print e
    Right i ->
      withSocketsDo $ WS.runClientWith
      "persienner.tangen6.dk"
      443
      "/scada-vis/objects/ws?auth=tangen:51e9b80e5f8affb493b045b31d0a2458"
      connectionOptions
      headers
      (wsApp i)
 where
  connectionOptions = WS.defaultConnectionOptions

  headers :: WS.Headers
  headers =
    [ (CI.mk "Authorization"        , "Basic dGFuZ2VuOlRhbmdlbjgyMDA=")
    , (CI.mk "Connection"           , "Upgrade")
    , (CI.mk "Origin"               , "https://persienner.tangen6.dk")
    , (CI.mk "Pragma"               , "no-cache")
    , (CI.mk "Cache-Control"        , "no-cache")
    , (CI.mk "Sec-WebSocket-Version", "13")
    , (CI.mk "Accept-Encoding"      , "gzip, deflate, br")
    , (CI.mk "Sec-WebSocket-Key"    , "l2RmxJj64ZTYMWmoZTqa1w==")
    , ( CI.mk "Sec-WebSocket-Extensions"
      , "permessage-deflate; client_max_window_bits"
      )
    ]
