{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Eulerplate.Renderer where

import           Data.Text hiding (init, length)
import           Eulerplate.Parser
import           Eulerplate.Types
import           Text.Casing           (camel)
import           Text.Shakespeare.Text
import Turtle

generateChallenge :: Challenge -> IO ()
generateChallenge challenge@Challenge {..} = do
  mktree $ fromText folderPath
  writeTextFile (fromText filePath) (renderChallenge challenge)
  where
    folderPath = "./" <> intercalate "/" (init breadcrumbs) <> "/"
    filePath = folderPath <> title <> ".hs"

renderChallenge :: Challenge -> Text
renderChallenge Challenge {..} =
  [st|-- Eulerplate generated module for #{title}
-- Challenge url: #{url}.
module #{Data.Text.intercalate "." breadcrumbs} where

-- Write your solution in here.
-- We've attempted to parse the types from the problem sets,
-- but feel free to change it in case we messed up :)
#{camelTitle} :: #{renderTypeList " -> " inputTypes} -> #{renderTuple outputTypes}
#{camelTitle} = undefined

main :: IO ()
main = do
  #{renderTuple inputTypes} <- getInput
  printOutput $ #{camelTitle} #{renderArguments " " inputTypes}

getInput :: IO #{renderTuple inputTypes}
getInput = do
  #{renderInputs inputTypes}
  return #{renderTuple inputTypes}

printOutput :: #{renderTuple outputTypes} -> IO ()
printOutput #{renderArguments " " outputTypes} = do
  #{renderOutputs outputTypes}
|]
  where camelTitle = camel $ unpack title
