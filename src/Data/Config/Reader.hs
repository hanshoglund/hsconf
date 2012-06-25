-- |
-- Module    : Data.Config.Reader
-- Copyright : 2011 Magnus Therning, 2012 Hans Hoglund
-- License   : BSD3
--
-- Parser for configurations.
module Data.Config.Reader
    ( parse
    , ConfigReaderError(..)
    , ConfigParseResult
    ) where

import Control.Monad.Error
import qualified Text.ParserCombinators.Parsec as P

import Data.Config
import Data.Config.Reader.Internals

-- | Parser for a configuration contained in a 'String'.
parse :: String -> ConfigParseResult Config
parse s = let
        pr = P.parse confParserEmpty "ini" s
    in case pr of
        Left e -> throwError . ConfigParserError $ show e
        Right [] -> Right $ emptyConfig
        Right is -> buildConfig is
