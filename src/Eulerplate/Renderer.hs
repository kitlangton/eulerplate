{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Eulerplate.Renderer where

import           Data.Text
import           Eulerplate.Parser
import           Eulerplate.Types
import           Text.Casing           (camel)
import           Text.Shakespeare.Text

renderChallenge :: Challenge -> Text
renderChallenge Challenge {..} =
  [st|module #{Data.Text.intercalate "." breadcrumbs} where

#{camelTitle} :: #{renderTypeList " -> " inputTypes} -> (#{renderTypeList ", " outputTypes})
#{camelTitle} = undefined

main :: IO ()
main = do
  (#{renderArguments ", " inputTypes}) <- getInput
  printOutput $ #{camelTitle} #{renderArguments " " inputTypes}

getInput :: IO (#{renderTypeList ", " inputTypes})
getInput = do
  #{renderInputs inputTypes}
  return (#{renderArguments ", " inputTypes})

printOutput :: (#{renderTypeList ", " outputTypes}) -> IO ()
printOutput #{renderArguments " " outputTypes} = do
  print a
|]
  where camelTitle = camel $ unpack title
