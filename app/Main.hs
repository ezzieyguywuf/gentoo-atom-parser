{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parser (eitherParse, prettyPrintAtom)
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Trying to parse dev-lib/foo-1.0"
  TextIO.putStr $ case eitherParse "dev-lib/foo-1.0" of
                     Left msg -> msg
                     Right atom -> prettyPrintAtom atom
