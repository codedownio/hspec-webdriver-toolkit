{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Simple.Exceptions where

import Control.Concurrent
import Control.Exception.Lifted as EL
import Control.Monad
import Control.Retry
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import GHC.Stack
import qualified Network.Socket as N
import System.Directory
import System.FilePath
import System.IO
import System.Random (randomRIO)
import Test.Hspec.WebDriver.Simple.Screenshots
import Test.Hspec.WebDriver.Simple.Types
import Test.Hspec.WebDriver.Simple.Util
import Test.WebDriver
import Text.Printf

#ifndef mingw32_HOST_OS
-- Note: one day, if directory-1.3.1.0 or later is ever on Stackage, we can use System.Directory.createDirectoryLink
import System.Posix.Files (createSymbolicLink)
#else
import Data.String
import Shelly hiding (sleep, (</>), FilePath, run)
#endif


handleTestException :: (HasCallStack) => WdSessionWithLabels -> EL.SomeException -> IO ()
handleTestException sessionWithLabels@(WdSessionWithLabels {wdSession=(WdSession {..}), ..}) e = do
  let resultsDir = getResultsDir sessionWithLabels
  createDirectoryIfMissing True resultsDir

  -- Put the error message in the results dir
  writeFile (resultsDir </> "error_info.txt") (show e)

  saveScreenshots "error_screenshot" sessionWithLabels

  -- Update the failure counter
  failureNum <- modifyMVar wdFailureCounter $ \n -> return (n + 1, n)

  -- Make a symlink to the results dir in the "errors" folder
  let errorsDir = resultsDir </> "errors"
  createDirectoryIfMissing True errorsDir

  let paddedNum :: String = printf "%04d" failureNum
  let errorFolderName = [i|#{paddedNum}_|] <> head wdLabels
#ifdef mingw32_HOST_OS
  -- Windows is stupid about symlinks, let's just copy
  shelly $ silently $ cp_r (fromString resultsDir) (fromString (dir </> errorFolderName))
#else
  -- Make the symlink relative so that it still works when test results are tarballed
  catch (createSymbolicLink (".." </> makeRelative errorsDir resultsDir) (errorsDir </> errorFolderName))
        (\(e :: SomeException) -> putStrLn [i|Error: failed to create symlink on test exception: #{e}|])
#endif
