{-# LANGUAGE OverloadedStrings #-}
module Parser (eitherParse) where

-- | Base imports
import Data.Void (Void)
import Data.Text (Text)

-- | Third-party imports
import Text.Megaparsec (Parsec)

-- | Our Data definitions
-------------------------
type Parser = Parsec Void Text

-- | The fundamental data type that we're trying to parse
data Atom =
    Atom { category     :: Text
         , package_name :: Text
         , version      :: Text
         , suffixes     :: [Suffix]
         , revision     :: Maybe Text
         } deriving (Show)

-- | Per the gentoo Package Manager Specification, section 3.2
data Suffix =
      Alpha Text
    | Beta  Text
    | Pre   Text
    | RC    Text
    | P     Text
    deriving (Show)

-- | Function Definitions
-------------------------
eitherParse :: Text -> Either Text Atom
eitherParse _ = Left "Not implemented"
