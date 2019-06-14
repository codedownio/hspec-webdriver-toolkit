{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables, ViewPatterns #-}
module Main where

import Control.Monad.IO.Class
import Data.Default
import Data.String.Interpolate.IsString
import System.Directory
import System.FilePath
import Test.Hspec (hspec)
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Toolkit
import Test.WebDriver.Commands

beforeAction :: WdSession -> IO WdSession
beforeAction sess@(getLabels -> labels) = do
  putStrLn $ "beforeAction called with labels: " ++ show labels
  return sess

afterAction (getLabels -> labels) = do
  putStrLn $ "afterAction called with labels: " ++ show labels

-- beforeWith beforeAction $ after afterAction

tests :: SpecType
tests = describe "Basic widget tests" $ do
  describe "Basic editing" $ do
    it "does the first thing" $ \(getLabels -> labels) -> do
      putStrLn $ "Doing the first thing: " <> show labels

    it "does the second thing" $ \_ -> do
      putStrLn "Doing the first thing"

    it "starts a browser" $ runWithBrowser "browser1" $ do
      openPage "http://www.google.com"

    it "starts another browser" $ runWithBrowser "browser2" $ do
      openPage "http://www.yahoo.com"

main :: IO ()
main = do
  let testRoot = "/tmp/testroot"
  let toolsRoot = testRoot </> "test_tools"
  let runsRoot = testRoot </> "test_runs"
  createDirectoryIfMissing True toolsRoot
  runRoot <- getTestFolder runsRoot
  createDirectoryIfMissing True runRoot

  putStrLn [i|\n********** Test root: #{testRoot} **********|]

  let wdOptions = def { toolsRoot = toolsRoot
                      , runRoot = runRoot
                      , capabilities = chromeCapabilities
                      -- , runInsideXvfb = Just def
                      }

  -- hspec $ runWebDriver wdOptions (
  --   screenshotBeforeAndAfterTest .
  --   recordEntireVideo .
  --   recordIndividualVideos .
  --   saveWebDriverLogs .
  --   saveBrowserLogs
  --   ) tests
  hspec $ runWebDriver wdOptions (recordTestTiming . saveWebDriverLogs) tests
