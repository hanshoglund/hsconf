-- |
-- Module    : Data.Conf.Types
-- Copyright : 2011 Magnus Therning, 2012 Hans Hoglund
-- License   : BSD3
module Data.Conf.Types where

import qualified Data.Map as M


type OptionName = String
type OptionValue = String
type Section = M.Map OptionName OptionValue

type SectionName = (String, Maybe String)
type Config = M.Map SectionName Section



-- useful since Map doesn't have any Serial instance
cfgFromList :: [(SectionName, [(OptionName, OptionValue)])] -> Config
cfgFromList =  M.map (M.fromList) . M.fromList

cfgToList :: Config -> [(SectionName, [(OptionName, OptionValue)])]
cfgToList = M.toList . M.map (M.toList)