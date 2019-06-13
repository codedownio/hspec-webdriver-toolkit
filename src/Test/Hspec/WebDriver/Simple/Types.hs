{-# LANGUAGE TypeFamilies, InstanceSigs, RecordWildCards, ScopedTypeVariables, QuasiQuotes #-}

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
import Test.Hspec.Core.Spec
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W

type Browser = String

type SpecType = SpecWith WdSessionWithLabels

type Hooks = SpecType -> SpecType

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

data WdSession = WdSession { wdOptions :: WdOptions
                           , wdSessionMap :: MVar [(Browser, W.WDSession)]
                           , wdFailureCounter :: MVar Int
                           , wdConfig :: W.WDConfig }

data WdSessionWithLabels = WdSessionWithLabels { wdLabels :: [String]
                                               , wdSession :: WdSession }

data WdExample = WdExample { wdBrowser :: Browser
                           , wdAction :: W.WD () }
               | WdPending { wdPendingMsg :: Maybe String }
