{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Internal.Exceptions where

import Control.Concurrent
import Control.Exception.Lifted as EL
import Data.String.Interpolate.IsString
import GHC.Stack
import System.Directory
import System.FilePath
import Test.Hspec.WebDriver.Internal.Hooks.Screenshots
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import Text.Printf

#ifndef mingw32_HOST_OS
-- Note: one day, if directory-1.3.1.0 or later is ever on Stackage, we can use System.Directory.createDirectoryLink
import System.Posix.Files (createSymbolicLink)
#else
import Data.String
import Shelly hiding (sleep, (</>), FilePath, run)
#endif


handleTestException :: (HasCallStack) => WdSession -> EL.SomeException -> IO ()
handleTestException sessionWithLabels@(WdSession {wdOptions=(WdOptions {runRoot}), ..}) e = do
  let resultsDir = getResultsDir sessionWithLabels
  createDirectoryIfMissing True resultsDir

  -- Put the error message in the results dir
  writeFile (resultsDir </> "error_info.txt") (show e)

  saveScreenshots "error_screenshot" sessionWithLabels

  -- Update the failure counter
  failureNum <- modifyMVar wdFailureCounter $ \n -> return (n + 1, n)

  -- Make a symlink to the results dir in the "errors" folder
  let errorsDir = runRoot </> "errors"
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
