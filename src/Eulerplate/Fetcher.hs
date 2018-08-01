{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Eulerplate.Fetcher
  ( getChallenge
  )
where

import           Codec.Archive.Zip
import           Control.Lens                   ( (^.) )
import           Control.Monad.Managed
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL

import           Data.List                      ( find )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromJust
                                                )
import           Data.Text                     as T
                                                ( Text
                                                , filter
                                                , intercalate
                                                , isPrefixOf
                                                , pack
                                                , unpack
                                                )
import           Text.Casing                    ( pascal )
import           Data.Text.Encoding
import           Data.Text.Read
import           Data.Either                    ( fromRight )
import           Data.Char                      ( isDigit )
import           Filesystem.Path.CurrentOS      ( encodeString )
import           Network.Wreq
import           Text.HTML.TagSoup
import           Turtle                         ( liftIO
                                                , mktempdir
                                                , runManaged
                                                , (</>)
                                                )
import           Eulerplate.Parser
import           Eulerplate.Renderer
import           Eulerplate.Types

getChallenge :: MonadManaged m => String -> m Challenge
getChallenge challengeID = do
  testCases@(TestCase { input, output } : _) <- getTestCases challengeID
  (title, breadcrumbs) <- liftIO $ getTitleAndBreadcrumbs challengeID
  return Challenge
    { url         = challengeUrl challengeID
    , testCases   = testCases
    , title = pack . pascal . unpack . T.filter (/= ':') $ decodeUtf8 title
    , inputTypes  = input
    , outputTypes = output
    , breadcrumbs = pack
      .   pascal
      .   unpack
      .   T.filter (/= ':')
      .   decodeUtf8
      <$> breadcrumbs
    , description = ""
    }


getTitleAndBreadcrumbs :: String -> IO (BS.ByteString, [BS.ByteString])
getTitleAndBreadcrumbs challengeID = do
  tags <- parseTags <$> fetchPage (challengeUrl challengeID)
  let crumbs@(title : _) =
        reverse
          $   fromTagText
          .   (!! 1)
          <$> sections (~== ("<span class='breadcrumb-item-text'>" :: String))
                       tags
  return (title, reverse crumbs)

problemSetUrl challengeID =
  "https://www.hackerrank.com/rest/contests/master/challenges/"
    <> challengeID
    <> "/download_testcases"

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
  keys    <- M.keys <$> getEntries
  entries <- traverse getExample keys
  let names = getEntryName <$> keys
  return entries

getExample :: EntrySelector -> ZipArchive TestSource
getExample entryName = do
  entryContent <- decodeUtf8 <$> getEntry entryName
  let name               = getEntryName entryName
  let entryType          = if T.isPrefixOf "input" name then Input else Output
  let Right (entryId, _) = decimal (T.filter isDigit name)
  return $ TestSource entryType entryId entryContent

organizeEntries :: [TestSource] -> [TestCase]
organizeEntries entries
  = let
      inputs  = Prelude.filter ((== Input) . sourceType) entries
      outputs = Prelude.filter ((== Output) . sourceType) entries
      sourceToCase source = do
        outputSource <- find ((sourceId source ==) . sourceId) outputs
        return TestCase
          { testCaseId = sourceId source
          , input = fromRight [] . parseTypes . sourceContent $ source
          , output = fromRight [] . parseTypes . sourceContent $ outputSource
          }
    in
      catMaybes $ sourceToCase <$> inputs

getTestCases :: MonadManaged m => String -> m [TestCase]
getTestCases challengeID = do
  problemZip <- liftIO $ fetchPage (problemSetUrl challengeID)
  directory  <- mktempdir "." "eulerplate"
  let target = encodeString $ directory </> "problem_set.zip"
  liftIO $ BS.writeFile target problemZip
  liftIO $ do
    entries <- withArchive target getData
    return $ organizeEntries entries
