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

data WdOptions = WdOptions {
  toolsRoot :: FilePath
  -- ^ Folder where any necessary binaries (chromedriver, Seleniu, etc.) will be downloaded if needed

  , runRoot :: FilePath
  -- ^ Folder where information for a specific run should be kept. Defaults to testRoot </> "test_runs" </> timestamp

  , skipRemainingTestsAfterFailure :: Bool
  -- ^ Whether to skip the rest of the tests once one fails

  , capabilities :: W.Capabilities
  -- ^ The WebDriver capabilities to use
  }

instance Default WdOptions where
  def = WdOptions "" "" True def

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
               | WdPending { wdPendingMsg :: Maybe String }

getLabels :: WdSession -> [String]
getLabels (WdSession {wdLabels}) = wdLabels
