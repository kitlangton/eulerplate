{-# LANGUAGE StrictData        #-}

module Eulerplate.Types where

import qualified Data.ByteString               as BS
import           Data.Text                     as T
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
