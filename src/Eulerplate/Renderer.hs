{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Eulerplate.Renderer where

import           Data.Text as T hiding (init, length)
import           Eulerplate.Parser
import           Eulerplate.Types
import           Text.Casing           (camel)
import           Text.Shakespeare.Text
import Turtle    hiding (input)

writeChallengeModule :: Text -> Challenge -> IO ()
writeChallengeModule projectPath challenge@Challenge {..} = do
  mktree $ fromText folderPath
  writeTextFile (fromText filePath) (renderChallenge challenge)
  where
    folderPath = projectPath <> "/src/" <> intercalate "/" (init breadcrumbs) <> "/"
    filePath = folderPath <> title <> ".hs"

writeSpecFile :: Text -> Challenge -> IO ()
writeSpecFile projectPath challenge@Challenge {..} = do
  mktree $ fromText folderPath
  writeTextFile (fromText filePath) (renderTests challenge)
  where
    folderPath = projectPath <> "/test/" <> intercalate "/" (init breadcrumbs) <> "/"
    filePath = folderPath <> title <> "Spec.hs"

renderTestCase :: Text -> TestCase -> Text
renderTestCase challengeTitle TestCase{..} =
  [st|it "solves Test Case ##{testCaseId}" $
    #{functionCall} `shouldBe` #{renderValueTuple output}
  |]
  where
  functionCall = renderFunction (pack . camel . unpack $challengeTitle) input

renderTests :: Challenge -> Text
renderTests Challenge{..} =
  [st|-- Eulerplate generated spec for #{title}
-- Challenge url: #{url}.
module #{T.intercalate "." breadcrumbs <> "Spec"} where

import #{T.intercalate "." breadcrumbs}
import Test.Hspec

spec :: SpecWith ()
spec = describe "#{title}" $ do
  #{intercalate "\n  " $ renderTestCase title <$> testCases}
  |]

renderChallenge :: Challenge -> Text
renderChallenge Challenge{..} =
  [st|-- Eulerplate generated module for #{title}
-- Challenge url: #{url}.
module #{T.intercalate "." breadcrumbs} where

-- Write your solution in here.
-- We've attempted to parse the types from the problem sets,
-- but feel free to change it in case we messed up :)
#{camelTitle} :: #{renderTypeList " -> " inputTypes} -> #{renderTuple outputTypes}
#{camelTitle} = undefined

main :: IO ()
main = do
  (#{renderArguments ", " inputTypes}) <- getInput
  printOutput $ #{camelTitle} #{renderArguments " " inputTypes}

getInput :: IO #{renderTuple inputTypes}
getInput = do
  #{renderInputs inputTypes}
  return (#{renderArguments ", " inputTypes})

printOutput :: #{renderTuple outputTypes} -> IO ()
printOutput (#{renderArguments ", " outputTypes}) = do
  #{renderOutputs outputTypes}
|]
  where camelTitle = camel $ unpack title
