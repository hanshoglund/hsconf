-- |
-- Module    : Data.Conf.Reader
-- Copyright : 2011 Magnus Therning, 2012 Hans Hoglund
-- License   : BSD3
--
-- Parser for configurations.
module Data.Conf.Reader
    ( parse
    , IniReaderError(..)
    , IniParseResult
    ) where

import Control.Monad.Error
import qualified Text.ParserCombinators.Parsec as P

import Data.Conf.Types
import Data.Conf.Reader.Internals

-- | Parser for a configuration contained in a 'String'.
parse :: String -> IniParseResult Config
parse s = let
        pr = P.parse iniParser "ini" s
    in case pr of
        Left e -> throwError . IniParserError $ show e
        Right is -> buildConfig is
