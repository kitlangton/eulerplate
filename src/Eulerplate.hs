{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}

module Eulerplate
  ( downloadChallenge
  , setProject
  , setUpProject
  )
where

import           Control.Monad.Managed
import           Eulerplate.Renderer
import           Eulerplate.Fetcher
import           Turtle                  hiding ( header
                                                , err
                                                )
import           Control.Monad.Except

data AppError = NoConfig

newtype App a = App { unApp :: ExceptT AppError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError)

runApp :: App () -> IO ()
runApp app = either handleError return =<< runExceptT (unApp app)

handleError :: AppError -> IO ()
handleError err = case err of
  NoConfig -> do
    print "You don't have a Haskell project path configured."
    print "Run 'eulerplate --init' to create a new project."
    print "(or run 'eulerplate --set-project' in an extant project directory)"

configPath = "~/.eulerplate"

getConfigPath :: IO Turtle.FilePath
getConfigPath = (</> fromText ".eulerplate") <$> home

setProjectPath :: Text -> IO ()
setProjectPath text = flip writeTextFile text =<< getConfigPath

setProject :: IO ()
setProject = do
  Right currentPath <- toText <$> pwd
  setProjectPath currentPath

setUpProject :: IO ()
setUpProject = do
  print "Setting up Eulerplate project"
  _ <- shell
    "stack new hacker-rank-hs https://github.com/kitlangton/eulerplate/raw/master/eulerplate-template.hsfiles"
    empty
  cd "hacker-rank-hs"
  setProject
  print "Successfully created hacker-rank-hs"
  runApp (liftIO . print =<< getProjectPath)

getProjectPath :: App Text
getProjectPath = do
  config     <- liftIO getConfigPath
  pathExists <- liftIO $ testfile config
  unless pathExists $ throwError NoConfig
  liftIO $ readTextFile config

downloadChallenge :: String -> IO ()
downloadChallenge challengeID = runApp $ do
  projectPath <- getProjectPath
  liftIO $ runManaged $ do
    liftIO $ print "Downloading Challenge"
    challenge <- getChallenge challengeID
    liftIO $ writeChallengeModule projectPath challenge
    liftIO $ writeSpecFile projectPath challenge

testRun = downloadChallenge "the-birthday-bar"
