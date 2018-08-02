{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Eulerplate
import           Options.Applicative

-- Option Parsing
type ChallengeID = String
data Command = NewProject | SetProject | DownloadChallenge ChallengeID

commandParser :: Parser Command
commandParser =
  downloadChallengeParser <|> setProjectParser <|> newProjectParser

setProjectParser :: Parser Command
setProjectParser = flag'
  SetProject
  (  long "set-project"
  <> short 's'
  <> help
       "Call from the root of the Haskell project you wish to use as your solution repo."
  )

newProjectParser :: Parser Command
newProjectParser = flag'
  NewProject
  (long "new" <> short 'n' <> help
    "Creates a new Haskell project for your Hacker Rank solutions."
  )

downloadChallengeParser :: Parser Command
downloadChallengeParser = DownloadChallenge <$> strOption
  (long "download" <> short 'd' <> metavar "CHALLENGE_ID" <> help
    "Creates a module and spec for the given Hacker Rank challenge id."
  )


run :: Command -> IO ()
run NewProject                      = setUpProject
run SetProject                      = setProject
run (DownloadChallenge challengeID) = downloadChallenge challengeID

main :: IO ()
main = run =<< execParser opts
 where
  opts = info
    commandParser
    (  fullDesc
    <> progDesc "A tool for bootstrapping Hacker Rank challenges"
    <> header "eulerplate - boilerplate for code challenges"
    )
