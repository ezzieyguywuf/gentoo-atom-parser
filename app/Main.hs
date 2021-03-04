{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Parser (eitherParse, prettyPrintAtom)
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  let packages = [ "dev-lib/foo-1.0"
                 , "dev-lib/bar-100dpi-2.0"
                 , "dev-lib/baz-3g"
                 ]
  mapM_ runParser packages

runParser :: Text -> IO ()
runParser text = do
    TextIO.putStrLn ("Trying to parse " <> text)
    TextIO.putStr $ case eitherParse text of
                     Left msg -> msg
                     Right atom -> prettyPrintAtom atom
    putStrLn ""
    putStrLn ""
