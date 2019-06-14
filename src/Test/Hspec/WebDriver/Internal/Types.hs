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

data WhenToSave = Always | OnException | Never deriving (Show, Eq)

data WdOptions = WdOptions {
  toolsRoot :: FilePath
  -- ^ Folder where any necessary binaries (chromedriver, Seleniu, etc.) will be downloaded if needed

  , runRoot :: FilePath
  -- ^ Folder where information for a specific run should be kept. Defaults to testRoot </> "test_runs" </> timestamp

  , skipRemainingTestsAfterFailure :: Bool
  -- ^ Whether to skip the rest of the tests once one fails

  , capabilities :: W.Capabilities
  -- ^ The WebDriver capabilities to use

  , saveSeleniumMessageHistory :: WhenToSave
  -- ^ When to save a record of Selenium requests and responses

  , runInsideXvfb :: Maybe XvfbConfig
  }

data XvfbConfig = XvfbConfig {
  xvfbResolution :: Maybe (Int, Int)
  -- ^ Resolution for the virtual screen. Defaults to (1920, 1080)
  }

instance Default XvfbConfig where
  def = XvfbConfig Nothing

instance Default WdOptions where
  def = WdOptions "" "" True def OnException Nothing

data WdSession = WdSession { wdLabels :: [String]
                           , wdWebDriver :: (Handle, Handle, ProcessHandle, FilePath, FilePath)
                           , wdOptions :: WdOptions
                           , wdSessionMap :: MVar [(Browser, W.WDSession)]
                           , wdFailureCounter :: MVar Int
                           , wdEntireTestRunVideo :: MVar (Maybe (Handle, Handle, ProcessHandle))
                           , wdTimingInfo :: MVar A.Value
                           , wdConfig :: W.WDConfig }

data WdExample = WdExample { wdBrowser :: Browser
                           , wdAction :: W.WD () }
               | WdExampleEveryBrowser { wdAction :: W.WD () }
               | WdPending { wdPendingMsg :: Maybe String }

getLabels :: WdSession -> [String]
getLabels (WdSession {wdLabels}) = wdLabels
