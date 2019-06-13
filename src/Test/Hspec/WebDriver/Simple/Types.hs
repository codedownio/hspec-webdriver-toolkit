{-# LANGUAGE TypeFamilies, InstanceSigs, RecordWildCards, ScopedTypeVariables, QuasiQuotes, Rank2Types #-}

module Test.Hspec.WebDriver.Simple.Types where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import Data.Either
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.Stack
import System.IO
import System.Process
import Test.Hspec.Core.Spec
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W

type Browser = String

type SpecType = SpecWith WdSession

type Hook = (HasCallStack) => SpecType -> SpecType

data WdOptions = WdOptions {
  testRoot :: FilePath

  -- Defaults to testRoot </> "test_tools"
  , toolsDir :: Maybe FilePath

  -- Folder where information for a specific run should be kept.
  -- Defaults to testRoot </> "test_runs" </> timestamp
  , runRoot :: FilePath

  -- Whether to skip the rest of the tests once one fails
  , skipRemainingTestsAfterFailure :: Bool
  }

instance Default WdOptions where
  def = WdOptions "/tmp" Nothing "/tmp/test_run" True

data WdSession = WdSession { wdLabels :: [String]
                                               , wdOptions :: WdOptions
                                               , wdSessionMap :: MVar [(Browser, W.WDSession)]
                                               , wdFailureCounter :: MVar Int
                                               , wdEntireTestRunVideo :: MVar (Maybe (Handle, Handle, ProcessHandle))
                                               , wdConfig :: W.WDConfig }

data WdExample = WdExample { wdBrowser :: Browser
                           , wdAction :: W.WD () }
               | WdPending { wdPendingMsg :: Maybe String }

getLabels :: WdSession -> [String]
getLabels (WdSession {wdLabels}) = wdLabels
