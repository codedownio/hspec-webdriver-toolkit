
{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Simple.Hooks.Logs (
  saveBrowserLogs
  , failOnSevereBrowserLogs
  , failOnCertainBrowserLogs
  , saveWebDriverLogs
  ) where

import Control.Concurrent
import Control.Exception.Lifted as EL
import Control.Monad
import Control.Retry
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import qualified Network.Socket as N
import System.Directory
import System.FilePath
import System.IO
import System.Random (randomRIO)
import Test.Hspec
import Test.Hspec.WebDriver.Simple.Types
import Test.Hspec.WebDriver.Simple.Util
import Test.WebDriver
import Text.Printf


-- * Hooks

-- | Save the browser logs for each test to the results directory.
saveBrowserLogs :: Hook
saveBrowserLogs = after flushLogsToFile

-- | Fail a test when severe browser logs are found.
failOnSevereBrowserLogs :: Hook
failOnSevereBrowserLogs = failOnCertainBrowserLogs predicate
  where predicate (LogEntry {logLevel}) = logLevel == LogSevere

-- | Fail a test when logs matching a predicate are found.
failOnCertainBrowserLogs :: (LogEntry -> Bool) -> Hook
failOnCertainBrowserLogs predicate = undefined

-- | Save the webdriver logs for each test to the results directory.
saveWebDriverLogs :: Hook
saveWebDriverLogs = undefined

-- * Implementation

flushLogsToFile :: (HasCallStack) => WdSessionWithLabels -> IO ()
flushLogsToFile sessionWithLabels@(WdSessionWithLabels {..}) = handle (\(e :: EL.SomeException) -> putStrLn [i|Failed to get logs: #{e}|]) $ do
  let resultsDir = getResultsDir sessionWithLabels
  createDirectoryIfMissing True resultsDir

  -- Note: we can call getLogTypes to get the list of available log type strings, of which "browser" should be one
  -- Remote logging is only supported on some browsers, Chrome and Firefox according to SO.

  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(browser, sess) -> do
    logs <- runWD sess $ getLogs "browser"
    unless (null logs) $ do
      -- Write the normal logs
      writeLogsToFile (resultsDir </> [i|#{browser}_browser_logs.log|]) logs

      -- If any severe logs are present, write them to a separate file
      let severeLogs = [x | x <- logs, logLevel x == LogSevere
                          , not ("favicon.ico - Failed to load resource" `T.isInfixOf` logMsg x)]
      unless (null severeLogs) $ writeLogsToFile (resultsDir </> [i|#{browser}_severe_browser_logs.log|]) severeLogs

writeLogsToFile :: (HasCallStack) => (HasCallStack) => FilePath -> [LogEntry] -> IO ()
writeLogsToFile filename logs =
  withFile filename AppendMode $ \h ->
    forM_ logs $ \(LogEntry time level msg) ->
      T.hPutStrLn h [i|#{time}\t#{level}\t#{msg}|]
