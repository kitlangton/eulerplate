{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving        #-}


module Eulerplate
  ( downloadChallenge
  , setProject
  )
where

import           Control.Monad.Managed
import           Eulerplate.Renderer
import           Eulerplate.Fetcher
import           Data.Text                      ( unpack )
import           Turtle                  hiding ( header )
import           Control.Monad.Except
import           Data.Either

newtype Settings = Settings {
  projectPath :: Text
}

data AppError = NoConfig

newtype App a = App { unApp :: ExceptT AppError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError)

runApp :: App () -> IO ()
runApp app = either handleError return =<< runExceptT (unApp app)

handleError :: AppError -> IO ()
handleError err = case err of
  NoConfig -> do
    print "You don't have a Haskell project path configured."
    print "Run 'eulerplate --set-project' in a Haskell project directory."

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
  result <- shell "stack new hacker-rank-hs" empty
  print result


getProjectPath :: App Text
getProjectPath = do
  config     <- liftIO getConfigPath
  pathExists <- liftIO $ testfile config
  unless pathExists $ throwError NoConfig
  liftIO $ readTextFile config

downloadChallenge :: String -> IO ()
downloadChallenge challengeID = runApp $ do
  liftIO $ print "downloading Challenge"
  projectPath <- getProjectPath
  liftIO $ runManaged $ do
    challenge <- getChallenge challengeID
    liftIO . print $ challenge
    liftIO . putStrLn . unpack $ renderChallenge challenge
    liftIO . putStrLn . unpack $ renderTests challenge
    liftIO $ writeChallengeModule projectPath challenge
    liftIO $ writeSpecFile projectPath challenge

testRun = downloadChallenge "the-birthday-bar"
