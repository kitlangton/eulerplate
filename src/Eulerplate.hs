{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Eulerplate where

import           Control.Monad.Managed
import           Eulerplate.Renderer
import           Eulerplate.Fetcher
import           Data.Text                      ( unpack )

printModule :: String -> IO ()
printModule challengeID = runManaged $ do
  challenge <- getChallenge challengeID
  liftIO . print $ challenge
  liftIO . putStrLn . unpack $ renderChallenge challenge
  liftIO $ generateChallenge challenge

testRun = printModule "the-birthday-bar"
