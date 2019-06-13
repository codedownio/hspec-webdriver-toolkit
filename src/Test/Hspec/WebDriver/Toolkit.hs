{-# LANGUAGE TypeFamilies, InstanceSigs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

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
  ) where

import GHC.Stack
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Simple.Exceptions
import Test.Hspec.WebDriver.Simple.Lib
import Test.Hspec.WebDriver.Simple.Logs
import Test.Hspec.WebDriver.Simple.Screenshots
import Test.Hspec.WebDriver.Simple.Types
import Test.Hspec.WebDriver.Simple.Video


-- | A good default set of hooks: `screenshotBeforeAndAfterTest`, `recordErrorVideos`, and `saveBrowserLogs`.
defaultHooks :: (HasCallStack) => Hooks
defaultHooks = screenshotBeforeAndAfterTest
  . recordErrorVideos
  . saveBrowserLogs


-- | All possible test instrumentation.
allHooks :: (HasCallStack) => Hooks
allHooks = undefined

-- | Start a Selenium server and run a spec inside it.
-- Auto-detects the browser version and downloads the Selenium .jar file and driver executable if necessary.
runWebDriver :: WdOptions -> Hooks -> SpecWith WdSessionWithLabels -> IO ()
runWebDriver = undefined

-- | Same as runWebDriver, but runs the entire test session inside XVFB (https://en.wikipedia.org/wiki/Xvfb)
-- so that tests run in their own X11 display.
-- For development, this makes tests more reproducible because the screen resolution can be fixed, and it
-- avoids cluttering your system with browser windows.
-- This is also a great way to run Selenium tests on a CI server.
-- Linux only.
runWebDriverXvfb :: WdOptions -> IO ()
runWebDriverXvfb = undefined