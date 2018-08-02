{-# LANGUAGE StrictData        #-}

module Eulerplate.Types where

import           Data.Text
import           Eulerplate.Parser

data Challenge = Challenge
  { url         :: String
  , title       :: Text
  , breadcrumbs :: [Text]
  , description :: Text
  , inputTypes :: TypeList
  , outputTypes :: TypeList
  , testCases   :: [TestCase]
  } deriving (Show)

data TestCase = TestCase
  { testCaseId :: Int
  , input      :: TypeList
  , output     :: TypeList
  } deriving (Show)
