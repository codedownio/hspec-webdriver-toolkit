{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables, ViewPatterns #-}

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.String.Interpolate.IsString
import Data.Time.Clock
import Data.Time.Format
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Simple.Capabilities
import Test.Hspec.WebDriver.Simple.Util
import Test.Hspec.WebDriver.Toolkit
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W

beforeAction :: WdSession -> IO WdSession
beforeAction sess@(getLabels -> labels) = do
  putStrLn $ "beforeAction called with labels: " ++ show labels
  return sess

afterAction (getLabels -> labels) = do
  putStrLn $ "afterAction called with labels: " ++ show labels

tests :: SpecType
tests = describe "Basic widget tests" $ beforeWith beforeAction $ after afterAction $ do
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
  let runsRoot = testRoot </> "test_runs"
  createDirectoryIfMissing True runsRoot
  runRoot <- getTestFolder' runsRoot
  putStrLn [i|\n********** Test root: #{testRoot} **********|]

  let wdOptions = def { testRoot = testRoot
                      , runRoot = runRoot }

  -- runWebDriver wdOptions (screenshotBeforeAndAfterTest . recordEntireVideo . recordIndividualVideos . saveWebDriverLogs . saveBrowserLogs) tests
  runWebDriver wdOptions chromeCapabilities (recordTestTiming) tests
