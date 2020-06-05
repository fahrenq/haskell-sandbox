{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Web.Scotty

import           Data.Monoid                    ( mconcat )

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

  get "/not/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>NOT: Scotty, ", beam, " me up!</h1>"]
