-- |
-- Module    : Data.Config.Writer
-- Copyright : 2012 Hans Hoglund
-- License   : BSD3
--
-- Writer for configurations.
module Data.Config.Writer
    ( write
    ) where

import Text.Printf
import Data.Config


-- | Parser for a configuration contained in a 'String'.
write :: Config -> String
write conf = 
    let 
        writeSectionName :: SectionName -> String
        writeSectionName (sn, Nothing)  = printf "[%s]\n" sn
        writeSectionName (sn, Just ssn) = printf "[%s \"%s\"]\n" sn ssn
        
        writeOption :: (OptionName, OptionValue) -> String
        writeOption (on, ov)            = printf "%s = %s\n" on ov 
        
        in
        concatMap (\(sn, os) -> writeSectionName sn ++ concatMap writeOption os ++ "\n") (cfgToList conf)
        
        
        

