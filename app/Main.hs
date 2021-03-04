{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Parser (eitherParse)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ Parser.eitherParse "dev-lib/foo-1.0"
