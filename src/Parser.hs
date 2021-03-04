{-# LANGUAGE OverloadedStrings #-}
module Parser (eitherParse, prettyPrintAtom) where

-- | Base imports
import           Control.Applicative (empty, some)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text as Text
import           Data.Void           (Void)

-- | Third-party imports
import           Text.Megaparsec      (Parsec, parse, errorBundlePretty
                                      , choice, many)
import           Text.Megaparsec.Char (space1, char, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- | Our Data definitions
-------------------------
type Parser = Parsec Void Text

-- | The fundamental data type that we're trying to parse
data Atom =
    Atom { getCategory     :: Text
         , getPackageName  :: Text
         , getVersion      :: Text
         , getSuffixes     :: [Suffix]
         , getRevision     :: Maybe Text
         } deriving (Show)

-- | Per the gentoo Package Manager Specification, section 3.2
data Suffix =
      Alpha { getSuffix :: Text}
    | Beta  { getSuffix :: Text}
    | Pre   { getSuffix :: Text}
    | RC    { getSuffix :: Text}
    | P     { getSuffix :: Text}
    deriving (Show)

-- | Exported Function Definitions
-------------------------

-- | Either succesfully parse the input text, or return an helpful error message
eitherParse :: Text -> Either Text Atom
eitherParse text = either err Right (parse parseAtom "" text)
    where err = Left . Text.pack . errorBundlePretty

-- | Turns an Atom into pretty text
prettyPrintAtom :: Atom -> Text
prettyPrintAtom (Atom category package_name version suffixes maybeRevision) =
    Text.pack "Category = " <> category <> Text.pack "\n" <>
    Text.pack "Package Name = " <> package_name <> Text.pack "\n" <>
    Text.pack "Version = " <> version <> Text.pack "\n" <>
    Text.pack "suffixes = " <> (Text.intercalate "." . map getSuffix) suffixes <>
    Text.pack "revision = " <> fromMaybe (Text.pack "r0") maybeRevision

-- | Internal Function Definitions
-------------------------

-- | Parses a single atom
parseAtom :: Parser Atom
parseAtom = do
    category <- parseWord
    pure (Atom category "not_implemented" "not_implemented" [] Nothing)

-- | Parses a "word", i.e. a valid "name" per the gentoo PMS
parseWord :: Parser Text
parseWord = do
    firstChar <- parseFirst
    balance   <- Text.concat <$> many (choice [ parseFirst, parseExtra ])
    pure (firstChar <> balance)

-- | Parses what PMS allows as the first character in names [A-Za-z0-9_]
parseFirst :: Parser Text
parseFirst = Text.singleton <$> choice [ alphaNumChar, char '_']

-- | After the first character, the PMS allows these extra characters too
parseExtra :: Parser Text
parseExtra = Text.singleton <$> choice [char '-', char '.', char '+']

-- | Used to consume any whitespace
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 empty empty

-- | Use the given parser to parse a "lexeme", consuming any space after
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer
