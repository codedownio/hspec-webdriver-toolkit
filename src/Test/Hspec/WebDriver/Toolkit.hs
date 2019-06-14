{-# LANGUAGE TypeFamilies, InstanceSigs, ScopedTypeVariables, QuasiQuotes, LambdaCase, Rank2Types #-}

module Test.Hspec.WebDriver.Toolkit (
  -- * Main functions
  runWebDriver
  , runWebDriverXvfb

  -- * Hooks

  -- ** Default hook sets
  , defaultHooks
  , allHooks

  -- ** Screenshots
  , screenshotBeforeTest
  , screenshotAfterTest
  , screenshotBeforeAndAfterTest

  -- ** Video recording
  , recordEntireVideo
  , recordIndividualVideos
  , recordErrorVideos

  -- ** Log saving
  , saveBrowserLogs
  , failOnSevereBrowserLogs
  , failOnCertainBrowserLogs
  , saveWebDriverLogs

  -- ** Test timing
  , recordTestTiming

  -- * Test helpers
  , runWithBrowser
  , runWithBrowser'
  , runEveryBrowser
  , closeAllSessions
  , getTestFolder

  -- * Types
  , Hook
  , SpecType
  , Browser
  , WdSession
  , WdExample
  , getLabels
  , getResultsDir
  , WdOptions(..)

  , module Test.Hspec.WebDriver.Toolkit.Capabilities
  , module Test.Hspec.WebDriver.Toolkit.Expectations
  ) where

import Data.Time.Clock
import Data.Time.Format
import GHC.Stack
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Internal.Binaries
import Test.Hspec.WebDriver.Internal.Exceptions
import Test.Hspec.WebDriver.Internal.Hooks.Logs
import Test.Hspec.WebDriver.Internal.Hooks.Screenshots
import Test.Hspec.WebDriver.Internal.Hooks.Timing
import Test.Hspec.WebDriver.Internal.Hooks.Video
import Test.Hspec.WebDriver.Internal.Lib
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import Test.Hspec.WebDriver.Internal.WebDriver
import Test.Hspec.WebDriver.Internal.Wrap
import Test.Hspec.WebDriver.Toolkit.Capabilities
import Test.Hspec.WebDriver.Toolkit.Expectations
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import qualified Test.WebDriver.Commands as W
import qualified Test.WebDriver.Config as W


-- | A good default set of hooks: `screenshotBeforeAndAfterTest`, `recordErrorVideos`, and `saveBrowserLogs`.
defaultHooks :: Hook
defaultHooks = screenshotBeforeAndAfterTest
  . recordErrorVideos
  . saveBrowserLogs

-- | All possible test instrumentation.
allHooks :: Hook
allHooks = undefined

-- | Start a Selenium server and run a spec inside it.
-- Auto-detects the browser version and downloads the Selenium .jar file and driver executable if necessary.
runWebDriver :: WdOptions -> Hook -> SpecWith WdSession -> Spec
runWebDriver wdOptions hooks tests =
  beforeAll (startWebDriver wdOptions) $
  afterAll stopWebDriver $
  afterAll closeAllSessions $
  addLabelsToTree (\labels sessionWithLabels -> sessionWithLabels { wdLabels = labels }) $
  hooks $
  tests

-- | Same as runWebDriver, but runs the entire test session inside XVFB (https://en.wikipedia.org/wiki/Xvfb)
-- so that tests run in their own X11 display.
-- For development, this makes tests more reproducible because the screen resolution can be fixed, and it
-- avoids cluttering your system with browser windows.
-- This is also a great way to run Selenium tests on a CI server.
-- Linux only.
runWebDriverXvfb :: WdOptions -> W.Capabilities -> Hook -> SpecWith WdSession -> IO ()
runWebDriverXvfb = undefined

-- | Create a timestamp-named folder to contain the results of a given test run
getTestFolder :: FilePath -> IO FilePath
getTestFolder baseDir = do
  timestamp <- formatTime defaultTimeLocale "%FT%H.%M.%S" <$> getCurrentTime
  let testRoot = baseDir </> timestamp
  createDirectoryIfMissing True testRoot
  return testRoot
