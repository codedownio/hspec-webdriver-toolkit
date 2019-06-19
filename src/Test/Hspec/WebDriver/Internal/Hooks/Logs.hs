
{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Internal.Hooks.Logs (
  saveBrowserLogs
  , failOnSevereBrowserLogs
  , failOnCertainBrowserLogs
  , saveWebDriverLogs

  , seleniumOutFileName
  , seleniumErrFileName
  , saveBrowserLogsIfConfigured
  ) where

import Control.Concurrent
import Control.Exception.Lifted as EL
import Control.Monad
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO
import Test.Hspec
import Test.Hspec.WebDriver.Internal.Misc
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import Test.WebDriver


-- * Hooks

-- | Save the browser logs for each test to the results directory.
saveBrowserLogs :: Hook
saveBrowserLogs = beforeAllWith $ \(WdSession {wdSaveBrowserLogs}) ->
  modifyMVar_ wdSaveBrowserLogs $ const $ return True

-- | Fail a test when severe browser logs are found. Implies 'saveBrowserLogs'.
failOnSevereBrowserLogs :: Hook
failOnSevereBrowserLogs = failOnCertainBrowserLogs predicate
  where predicate (LogEntry {logLevel}) = logLevel == LogSevere

-- | Fail a test when logs matching a predicate are found. Implies 'saveBrowserLogs'.
failOnCertainBrowserLogs :: (LogEntry -> Bool) -> Hook
failOnCertainBrowserLogs predicate = beforeAllWith $ \(WdSession {wdLogFailureFn}) ->
  modifyMVar_ wdLogFailureFn $ const $ return predicate

-- | Save the webdriver logs for each test to the results directory. Implies 'saveBrowserLogs'.
saveWebDriverLogs :: Hook
saveWebDriverLogs = after $ \sess@(WdSession {wdWebDriver=(_, _, _, stdoutPath, stderrPath, _)}) -> do
  let resultsDir = getResultsDir sess
  createDirectoryIfMissing True resultsDir
  moveAndTruncate stdoutPath (resultsDir </> seleniumOutFileName)
  moveAndTruncate stderrPath (resultsDir </> seleniumErrFileName)

-- * Constants

seleniumErrFileName, seleniumOutFileName :: String
seleniumErrFileName = "selenium_stderr.log"
seleniumOutFileName = "selenium_stdout.log"

-- * Implementation

-- | The return value contains any log entries which failed the wdLogFailureFn.
flushLogsToFile :: (HasCallStack) => WdSession -> IO [LogEntry]
flushLogsToFile sessionWithLabels@(WdSession {..}) =
  handle (\(e :: EL.SomeException) -> putStrLn [i|Failed to get logs: #{e}|] >> return []) $ do
    let resultsDir = getResultsDir sessionWithLabels
    createDirectoryIfMissing True resultsDir

    -- Note: we can call getLogTypes to get the list of available log type strings, of which "browser" should be one
    -- Remote logging is only supported on some browsers, Chrome and Firefox according to SO.

    logFailureFn <- readMVar wdLogFailureFn
    sessionMap <- readMVar wdSessionMap
    failingLogs <- forM (M.toList sessionMap) $ \(browser, sess) -> do
      logs <- runWD sess $ getLogs "browser"
      unless (null logs) $ do
        -- Write the normal logs
        writeLogsToFile (resultsDir </> [i|#{browser}_browser_logs.log|]) logs

        -- If any severe logs are present, write them to a separate file
        let severeLogs = [x | x <- logs, logLevel x == LogSevere
                            , not ("favicon.ico - Failed to load resource" `T.isInfixOf` logMsg x)]
        unless (null severeLogs) $ writeLogsToFile (resultsDir </> [i|#{browser}_severe_browser_logs.log|]) severeLogs

      return $ filter logFailureFn logs

    return $ mconcat failingLogs

  where
    writeLogsToFile :: (HasCallStack) => (HasCallStack) => FilePath -> [LogEntry] -> IO ()
    writeLogsToFile filename logs =
      withFile filename AppendMode $ \h ->
        forM_ logs $ \(LogEntry time level msg) ->
          T.hPutStrLn h [i|#{time}\t#{level}\t#{msg}|]

-- | The return value contains any log entries which failed the wdLogFailureFn.
saveBrowserLogsIfConfigured :: WdSession -> IO [LogEntry]
saveBrowserLogsIfConfigured sess =
  handle (\(e :: SomeException) -> putStrLn [i|Error saving browser logs: '#{e}'|] >> return [])
         (flushLogsToFile sess)
