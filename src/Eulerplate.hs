{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Eulerplate where

import           Codec.Archive.Zip
import           Control.Lens              ((^.))
import           Control.Monad.Managed
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Char
import           Data.List
import Data.Either (fromRight)
import qualified Data.Map                  as M
import           Data.Maybe                (catMaybes, fromJust)
import           Data.Text                 (Text, filter, intercalate,
                                            isPrefixOf, pack, unpack)
import           Data.Text.Encoding
import           Data.Text.Read
import           Filesystem.Path.CurrentOS (encodeString)
import           Network.Wreq
import           Text.Casing               (pascal)
import           Text.HTML.TagSoup
import           Turtle                    (liftIO, mktempdir, runManaged,
                                            (</>))

import           Eulerplate.Renderer
import           Eulerplate.Types
import           Eulerplate.Parser

getTestCase :: IO TestCase
getTestCase = undefined

parseDescription :: IO String
parseDescription = undefined

getTitleAndBreadcrumbs :: String -> IO (BS.ByteString, [BS.ByteString])
getTitleAndBreadcrumbs challengeID = do
  tags <- parseTags <$> fetchPage (challengeUrl challengeID)
  let (title:crumbs) =
        reverse $
        fmap (fromTagText . head . drop 1) $
        sections (~== ("<span class='breadcrumb-item-text'>" :: String)) tags
  return (title, reverse crumbs)

exampleChallengeID = "insert-a-node-at-the-head-of-a-linked-list"

problemSetUrl challengeID =
  "https://www.hackerrank.com/rest/contests/master/challenges/" <> challengeID <>
  "/download_testcases"

challengeUrl challengeID =
  "https://www.hackerrank.com/challenges/" <> challengeID <> "/problem"

fetchPage :: String -> IO BS.ByteString
fetchPage url = do
  response <- get url
  return $ BSL.toStrict $ response ^. responseBody

data TestSource = TestSource
  { sourceType    :: TestSourceType
  , sourceId      :: Int
  , sourceContent :: Text
  } deriving (Show)

data TestSourceType
  = Input
  | Output
  deriving (Show, Eq)

getData :: ZipArchive [TestSource]
getData = do
  keys <- M.keys <$> getEntries
  entries <- traverse getExample keys
  let names = getEntryName <$> keys
  return entries

getExample :: EntrySelector -> ZipArchive TestSource
getExample entryName = do
  entryContent <- decodeUtf8 <$> getEntry entryName
  let name = getEntryName entryName
  let entryType =
        if Data.Text.isPrefixOf "input" name
          then Input
          else Output
  let Right (entryId, _) = decimal (Data.Text.filter isDigit name)
  return $ TestSource entryType entryId entryContent

organizeEntries :: [TestSource] -> [TestCase]
organizeEntries entries =
  let inputs = Prelude.filter ((== Input) . sourceType) entries
      outputs = Prelude.filter ((== Output) . sourceType) entries
      sourceToCase source = do
        outputSource <- find ((sourceId source ==) . sourceId) outputs
        return
          TestCase
            { testCaseId = sourceId source
            , input = sourceContent source
            , output = sourceContent outputSource
            }
   in catMaybes $ sourceToCase <$> inputs


getChallenge :: MonadManaged m => String -> m Challenge
getChallenge challengeID = do
  testCases@(TestCase{input, output}:_) <- getTestCases challengeID
  (title, breadcrumbs) <- liftIO $ getTitleAndBreadcrumbs challengeID
  return
    Challenge
      { url = challengeUrl challengeID
      , testCases = testCases
      , title = decodeUtf8 title
      , inputTypes = fromRight [] $ parseTypes input
      , outputTypes = fromRight [] $ parseTypes output
      , breadcrumbs = pack . pascal . unpack . decodeUtf8 <$> breadcrumbs
      , description = ""
      }

getTestCases :: MonadManaged m => String -> m [TestCase]
getTestCases challengeID = do
  problemZip <- liftIO $ fetchPage (problemSetUrl challengeID)
  directory <- mktempdir "." "eulerplate"
  let target = encodeString $ directory </> "problem_set.zip"
  liftIO $ BS.writeFile target problemZip
  liftIO $ do
    entries <- withArchive target getData
    return $ organizeEntries entries

testRun = do
  runManaged $ do
    challenge <- getChallenge "the-birthday-bar"
    liftIO . print $ challenge
    liftIO . putStrLn $ unpack $ renderChallenge challenge
