-- |
-- Module    : Data.Config.Reader
-- Copyright : 2011 Magnus Therning, 2012 Hans Hoglund
-- License   : BSD3
--
-- Parser for configurations.
module Data.Config.Reader (
        parse, 
        parse', 
        errorMessage, 
        ConfigReaderError(..), 
        ConfigParseResult
  ) where

import Control.Monad.Error
import qualified Text.ParserCombinators.Parsec as P

import Data.Config
import Data.Config.Reader.Internals

-- | Parse a configuration.
parse :: String -> ConfigParseResult Config
parse = parse' ""

-- | Parse a configuration from a file.
parse' :: FilePath -> String -> ConfigParseResult Config
parse' fileName s = let
        pr = P.parse confParserEmpty fileName s
    in case pr of
        Left e -> throwError . ConfigParserError $ show e
        Right [] -> Right $ emptyConfig
        Right is -> buildConfig is
