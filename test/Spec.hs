{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables #-}

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
import Test.Hspec.WebDriver.Simple.Binaries
import Test.Hspec.WebDriver.Simple.Lib
import Test.Hspec.WebDriver.Simple.Logs
import Test.Hspec.WebDriver.Simple.Screenshots
import Test.Hspec.WebDriver.Simple.Types
import Test.Hspec.WebDriver.Simple.Util
import Test.Hspec.WebDriver.Simple.Video
import Test.Hspec.WebDriver.Simple.Wrap
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W

beforeAction :: WdSessionWithLabels -> IO WdSessionWithLabels
beforeAction sess@(WdSessionWithLabels {wdLabels}) = do
  putStrLn $ "beforeAction called with labels: " ++ show wdLabels
  return sess

afterAction (WdSessionWithLabels {wdLabels}) = do
  putStrLn $ "afterAction called with labels: " ++ show wdLabels

tests :: SpecType
tests = describe "Basic widget tests" $ beforeWith beforeAction $ after afterAction $ do
  describe "Basic editing" $ do
    it "does the first thing" $ \(WdSessionWithLabels {..}) -> do
      putStrLn $ "Doing the first thing: " <> show wdLabels

    it "does the second thing" $ \(WdSessionWithLabels {..}) -> do
      putStrLn $ "Doing the first thing: " <> show wdLabels

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

  withWebDriver wdOptions $ \baseConfig webDriverLogSavingHooks -> do
    initialSessionWithLabels <- makeInitialSessionWithLabels wdOptions baseConfig $ W.defaultCaps { W.browser = W.chrome }

    hspec $ beforeAll (return initialSessionWithLabels) $
      afterAll closeAllSessions $
      addLabelsToTree (\labels sessionWithLabels -> sessionWithLabels { wdLabels = labels }) $
      screenshotBeforeAndAfterTest $
      recordEntireVideo $
      recordIndividualVideos $
      webDriverLogSavingHooks $
      saveBrowserLogs $
      tests
