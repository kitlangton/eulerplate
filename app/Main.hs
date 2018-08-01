{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Eulerplate
import           Options.Applicative
import           Data.Functor                   ( ($>) )

-- Option Parsing
type ChallengeID = String
data Command = SetProject | DownloadChallenge ChallengeID

commandParser :: Parser Command
commandParser = downloadChallengeParser <|> setProjectParser

setProjectParser :: Parser Command
setProjectParser = flag'
  SetProject
  (  long "set-project"
  <> short 's'
  <> help
       "Call from the root of the Haskell project you wish to use as your solution repo."
  )

downloadChallengeParser :: Parser Command
downloadChallengeParser = DownloadChallenge <$> strOption
  (long "download" <> short 'd' <> metavar "CHALLENGE_ID" <> help
    "Creates a module and spec for the given Hacker Rank challenge id."
  )


run :: Command -> IO ()
run SetProject                      = setProject
run (DownloadChallenge challengeID) = downloadChallenge challengeID

main :: IO ()
main = do
  print "Running"
  run =<< execParser opts
  print "Ran"
 where
  opts = info
    commandParser
    (  fullDesc
    <> progDesc "A tool for bootstrapping Hacker Rank challenges"
    <> header "eulerplate - boilerplate for code challenges"
    )
