{-# LANGUAGE OverloadedStrings #-}
module Parser (eitherParse, prettyPrintAtom) where

-- | Base imports
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text as Text
import           Data.Void           (Void)

-- | Third-party imports
import           Text.Megaparsec      (Parsec, parse, errorBundlePretty
                                      , empty, some, choice, many, optional
                                      , someTill_, try, sepBy, manyTill)
import           Text.Megaparsec.Char (space1, char, alphaNumChar, digitChar
                                      , lowerChar)
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
    category    <- parseCategory
    -- packageName <- parseWord
    -- version <- parseVersionNumber
    pure (Atom category "not_implemented" "not_implemented" [] Nothing)

-- | Parses the category, which precedes the '/'
parseCategory :: Parser Text
parseCategory = do
    firstLetter <- parseFirstChar
    moreLetters <- Text.concat <$> manyTill parseAfterChar (char '/')
    pure (firstLetter <> moreLetters)

-- | Parses what PMS allows as the first character in names [A-Za-z0-9_]
parseFirstChar :: Parser Text
parseFirstChar = Text.singleton <$> choice [ alphaNumChar, char '_']

-- | After the first character, the PMS allows these extra characters too
parseAfterChar :: Parser Text
parseAfterChar = choice [ parseFirstChar
                        , Text.singleton <$> char '-'
                        , Text.singleton <$> char '.'
                        , Text.singleton <$> char '+'
                        ]

-- | Parses the numeric portion of a version
parseVersionNumber :: Parser Text
parseVersionNumber = do
    firstPart <- parseInteger
    -- many returns zero or more matches
    backEnd   <- many  (char '.' >> Text.append "." <$> parseInteger)
    -- empty string or single lower-chase character
    suffix    <- maybe "" Text.singleton <$> optional lowerChar
    -- glue it all back together
    pure (Text.concat (firstPart : (backEnd <> [suffix])))

-- | Parses...an integer
parseInteger :: Parser Text
parseInteger = Text.pack <$> some digitChar

-- | Used to consume any whitespace
spaceConsumer :: Parser ()
spaceConsumer = Lexer.space space1 empty empty

-- | Use the given parser to parse a "lexeme", consuming any space after
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer
