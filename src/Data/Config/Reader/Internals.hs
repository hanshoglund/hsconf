-- |
-- Module    : Data.Config.Reader.Internals
-- Copyright : 2011 Magnus Therning, 2012 Hans Hoglund
-- License   : BSD3
--
-- Internal functions used in 'Data.Config.Reader'.
module Data.Config.Reader.Internals where

import Control.Monad.Error
import Control.Monad.State
-- import Text.ParserCombinators.Parsec as P
import Text.Parsec as P
import Text.Parsec.String
import qualified Data.ByteString as BS

import Data.Config

data ConfigReaderError
    = ConfigParserError String
    | ConfigSyntaxError String
    | ConfigOtherError String
    deriving (Eq, Show)

instance Error ConfigReaderError where
    noMsg = ConfigOtherError "Unknown error"
    strMsg s = ConfigOtherError s

errorMessage :: ConfigReaderError -> String
errorMessage (ConfigOtherError x)  = x
errorMessage (ConfigParserError x) = x
errorMessage (ConfigSyntaxError x) = x

type ConfigParseResult = Either ConfigReaderError

-- | The type used to represent a line of a config file.
data ConfigFileLine
    = SectionL String (Maybe String)
    | OptionL String String
    | OptionContL String
    | CommentL
    deriving (Show, Eq)

-- | Build a configuration from a list of 'ConfigFileLine' items.
buildConfig :: [ConfigFileLine] -> ConfigParseResult Config
buildConfig ifs = let
        isComment CommentL = True
        isComment _        = False

        nonCommentConfigFileLines = filter (not . isComment) ifs

        -- merge together OptionL and subsequent OptionContL items
        mergeOptions :: [ConfigFileLine] -> Either ConfigReaderError [ConfigFileLine]
        mergeOptions [] = return []
        mergeOptions (s@(SectionL _ _) : ifs)                = (s :) `liftM` mergeOptions ifs
        mergeOptions (CommentL : ifs )                       = (CommentL :) `liftM` mergeOptions ifs
        mergeOptions (OptionL on ov : OptionContL ov2 : ifs) = mergeOptions $ (OptionL on (ov ++ ov2)) : ifs
        mergeOptions (o@(OptionL on ov) : ifs)               = (o :) `liftM` mergeOptions ifs
        mergeOptions _ = throwError $ ConfigSyntaxError "Syntax error in configuration file."

        -- build the configuration from a [ConfigFileLine]
        buildit a [] = return a
        buildit a (SectionL sn ssn : is) = put (sn, ssn) >> buildit a is
        buildit a (OptionL on ov : is) = do
            sn <- get
            let na = setOption sn on ov a
            buildit na is

    in mergeOptions nonCommentConfigFileLines >>= (\ is -> return . fst $ runState (buildit emptyConfig is) defaultSectionName)

-- | Consumer of whitespace \"@ \t@\".
eatWhiteSpace :: Parser String
eatWhiteSpace = many $ oneOf " \t"

endOfLine :: Parser ()
endOfLine = (newline >> return ()){- <|> (eof >> return ()) -}

-- | Parser for the start-of-section line.  It expects the line to start with a
-- @[@ then find the section name, and finally a @]@.  The section name may be
-- surrounded by any number of white space characters (see 'eatWhiteSpace').
secParser :: Parser ConfigFileLine
secParser = let
        validSecNameChrs    = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-/@"
        validSubSecNameChrs = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-/@"
    in do
        char '['
        eatWhiteSpace
        sn <- many1 $ oneOf validSecNameChrs
        eatWhiteSpace
        ssn <- optionMaybe $ do
            char '"'
            ssn <- many1 $ oneOf validSubSecNameChrs
            char '"'
            return ssn
        char ']'
        manyTill anyChar endOfLine
        return $ SectionL sn ssn

-- | Parser for a single line of an option.  The line must start with an option
-- name, then a @=@ must be found, and finally the rest of the line is taken as
-- the option value.  The equal sign may be surrounded by any number of white
-- space characters (see 'eatWhiteSpace').
optLineParser :: Parser ConfigFileLine
optLineParser = let
        validOptNameChrs = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-/@"
    in do
        eatWhiteSpace
        on <- many1 $ oneOf validOptNameChrs
        eatWhiteSpace
        char '='
        eatWhiteSpace
        ov <- manyTill anyChar endOfLine
        return $ OptionL on ov

-- | Parser for an option-value continuation line.  The line must start with
-- either a space or a tab character (\"@ \t@\").  Everything else on the line,
-- until the newline character, is taken as the continuation of an option
-- value.
optContParser :: Parser ConfigFileLine
optContParser = do
    oneOf " \t"
    eatWhiteSpace
    oc <- noneOf " \t"
    ov <- manyTill anyChar endOfLine
    return $ OptionContL $ oc:ov

-- | Parser for "noise" in the configuration file, such as comments and empty
-- lines.  (Note that lines containing only space characters will be
-- successfully parsed by 'optContParser'.)
noiseParser :: Parser ConfigFileLine
noiseParser = let
        commentP = do
            oneOf "#;"
            manyTill anyChar endOfLine
        emptyL = endOfLine >> return ""
    in choice [commentP, emptyL] >> return CommentL

confParser :: Parser [ConfigFileLine]
confParser = do
    many noiseParser
    s1 <- secParser
    r <- many $ choice [secParser, optLineParser, optContParser, noiseParser]
    return (s1:r)

-- Like confParser, but may return empty string
confParserEmpty :: Parser [ConfigFileLine]
confParserEmpty = do
    many noiseParser
    do { eof; return [] }
    <|>
    do
        s1 <- secParser
        r <- many $ choice [secParser, optLineParser, optContParser, noiseParser]
        return (s1:r)
            
    
    