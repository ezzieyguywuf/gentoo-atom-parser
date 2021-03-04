{-# LANGUAGE OverloadedStrings #-}
module Parser (eitherParse) where

-- | Base imports
import Data.Void           (Void)
import Data.Text           (Text, pack)
import Control.Applicative (empty)

-- | Third-party imports
import           Text.Megaparsec      (Parsec, parse, errorBundlePretty)
import           Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as Lexer

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

-- | Either succesfully parse the input text, or return an helpful error message
eitherParse :: Text -> Either Text Atom
eitherParse text = either err Right (parse parseAtom "" text)
    where err = Left . pack . errorBundlePretty

-- | Parses a single atom
parseAtom :: Parser Atom
parseAtom = pure (Atom "not" "implemented" "1.0.0" [] Nothing)

-- | Used to consume any whitespace
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 empty empty

-- | Use the given parser to parse a "lexeme", consuming any space after
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer
