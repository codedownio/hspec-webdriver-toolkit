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
import Test.Hspec.WebDriver.Toolkit
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W

tests :: SpecType
tests = describe "Tookit tests" $ do
  it "works" $ \_ -> pending

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
                      , capabilities = chromeCapabilities }

  -- hspec $ runWebDriver wdOptions (screenshotBeforeAndAfterTest . recordEntireVideo . recordIndividualVideos . saveWebDriverLogs . saveBrowserLogs) tests
  hspec $ runWebDriver wdOptions (recordTestTiming) tests
