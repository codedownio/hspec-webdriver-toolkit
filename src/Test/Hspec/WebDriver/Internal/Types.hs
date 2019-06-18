{-# LANGUAGE TypeFamilies, InstanceSigs, RecordWildCards, ScopedTypeVariables, QuasiQuotes, Rank2Types #-}

module Test.Hspec.WebDriver.Internal.Types where

import Control.Concurrent.MVar
import qualified Data.Aeson as A
import Data.Default
import GHC.Stack
import System.IO
import System.Process
import Test.Hspec.Core.Spec
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Session as W

type Browser = String

type SpecType = SpecWith WdSession

type Hook = (HasCallStack) => SpecType -> SpecType

type ToolsRoot = FilePath
type RunRoot = FilePath

data WhenToSave = Always | OnException | Never deriving (Show, Eq)

-- | Headless and Xvfb modes are useful because they allow you to run tests in the background, without popping up browser windows.
-- This is useful for development or for running on a CI server, and is also more reproducible since the screen resolution can be fixed.
-- In addition, Xvfb mode allows videos to be recorded of tests.
data RunMode = Normal
             -- ^ Normal Selenium behavior; will pop up a web browser.
             | RunHeadless
             -- ^ Run with a headless browser. Supports screenshots but videos will be black.
             | RunInXvfb XvfbConfig
             -- ^ Run inside <https://en.wikipedia.org/wiki/Xvfb Xvfb> so that tests run in their own X11 display.
             -- xvfb-run script must be installed and on the PATH.

data WdOptions = WdOptions {
  toolsRoot :: ToolsRoot
  -- ^ Folder where any necessary binaries (chromedriver, Selenium, etc.) will be downloaded if needed. Required.

  , runRoot :: RunRoot
  -- ^ Folder where information for a specific run should be kept. Required.

  , logFailureFn :: W.LogEntry -> Bool
  -- ^ A function to apply to browser logs at the end of every test. If it returns true, the test is failed. Can be used to fail tests if certain browser logs are found (for example, all 'LogSevere' logs). Defaults to @const False@.

  , capabilities :: W.Capabilities
  -- ^ The WebDriver capabilities to use

  , saveSeleniumMessageHistory :: WhenToSave
  -- ^ When to save a record of Selenium requests and responses

  , runMode :: RunMode
  -- ^ How to handle opening the browser (in a popup window, headless, etc.)
  }

data XvfbConfig = XvfbConfig {
  xvfbResolution :: Maybe (Int, Int)
  -- ^ Resolution for the virtual screen. Defaults to (1920, 1080)
  }

instance Default XvfbConfig where
  def = XvfbConfig Nothing

defaultWdOptions :: FilePath -> FilePath -> WdOptions
defaultWdOptions toolsRoot runRoot = WdOptions toolsRoot runRoot (const False) def OnException Normal

data WdSession = WdSession { wdLabels :: [String]
                           , wdWebDriver :: (Handle, Handle, ProcessHandle, FilePath, FilePath)
                           , wdOptions :: WdOptions
                           , wdSessionMap :: MVar [(Browser, W.WDSession)]
                           , wdFailureCounter :: MVar Int
                           , wdEntireTestRunVideo :: MVar (Maybe (Handle, Handle, ProcessHandle))
                           , wdTimingInfo :: MVar A.Value
                           , wdLogFailureFn :: MVar (W.LogEntry -> Bool)
                           , wdConfig :: W.WDConfig }

data WdExample = WdExample { wdBrowser :: Browser
                           , wdAction :: W.WD () }
               | WdExampleEveryBrowser { wdAction :: W.WD () }
               | WdPending { wdPendingMsg :: Maybe String }
