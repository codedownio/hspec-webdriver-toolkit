{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, LambdaCase, Rank2Types #-}

module Test.Hspec.WebDriver.Internal.WebDriver (
  startWebDriver
  , stopWebDriver
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Except
import Control.Retry
import qualified Data.Aeson as A
import Data.Default
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (PortNumber)
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Test.Hspec.WebDriver.Internal.Binaries
import Test.Hspec.WebDriver.Internal.Hooks.Logs
import Test.Hspec.WebDriver.Internal.Ports
import Test.Hspec.WebDriver.Internal.Types
import qualified Test.WebDriver.Config as W

-- | Spin up a Selenium WebDriver and create a WdSession
startWebDriver :: WdOptions -> IO WdSession
startWebDriver wdOptions@(WdOptions {toolsRoot, runRoot, capabilities}) = do
  -- Set up config
  port <- findFreePortOrException
  let wdConfig = (def { W.wdPort = fromIntegral port, W.wdCapabilities = capabilities })

  -- Get the CreateProcess
  createDirectoryIfMissing True toolsRoot
  wdCreateProcess <- getWebdriverCreateProcess wdOptions port >>= \case
    Left err -> error [i|Failed to create webdriver process: '#{err}'|]
    Right x -> return x

  -- Open output handles
  let logsDir = runRoot </> "selenium_logs"
  createDirectoryIfMissing True logsDir
  hout <- openFile (logsDir </> seleniumOutFileName) AppendMode
  herr <- openFile (logsDir </> seleniumErrFileName) AppendMode

  putStrLn "Waiting for selenium server"

  -- Start the process and wait for it to be ready
  (_, _, _, p) <- createProcess $ wdCreateProcess {
    std_in = Inherit
    , std_out = UseHandle hout
    , std_err = UseHandle herr
    }
  -- Normally Selenium prints the ready message to stderr. However, when we're running under
  -- XVFB the two streams get combined and sent to stdout; see
  -- https://bugs.launchpad.net/ubuntu/+source/xorg-server/+bug/1059947
  -- As a result, we poll both files
  let readyMessage = "Selenium Server is up and running"
  -- Retry every 60ms, for up to 60s before admitting defeat
  let retryPolicy = constantDelay 60000 <> limitRetries 1000
  success <- retrying retryPolicy (\_retryStatus result -> return (not result)) $ const $
    T.readFile (logsDir </> seleniumErrFileName) >>= \case
      t | readyMessage `T.isInfixOf` t -> return True
      _ -> T.readFile (logsDir </> seleniumOutFileName) >>= \case
        t | readyMessage `T.isInfixOf` t -> return True
        _ -> return False
  unless success $ error [i|Selenium server failed to start after 60 seconds|]
  putStrLn "Selenium server started!"

  -- Make the WdSession
  WdSession <$> pure []
            <*> pure (hout, herr, p, logsDir </> seleniumOutFileName, logsDir </> seleniumErrFileName)
            <*> pure wdOptions
            <*> newMVar []
            <*> newMVar 0
            <*> newMVar Nothing
            <*> newMVar (A.object [])
            <*> pure wdConfig


stopWebDriver :: WdSession -> IO ()
stopWebDriver (WdSession {wdWebDriver=(hout, herr, h, _, _)}) = do
  terminateProcess h >> waitForProcess h
  hClose hout
  hClose herr


-- * Util

getWebdriverCreateProcess :: WdOptions -> PortNumber -> IO (Either T.Text CreateProcess)
getWebdriverCreateProcess (WdOptions {toolsRoot, runInsideXvfb}) port = runExceptT $ do
  chromeDriverPath <- ExceptT $ downloadChromeDriverIfNecessary toolsRoot
  seleniumPath <- ExceptT $ downloadSeleniumIfNecessary toolsRoot

  case runInsideXvfb of
    Nothing -> return (proc "java" [
                          [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                            , "-jar", seleniumPath
                            , "-port", show port
                            ])

    Just (XvfbConfig { xvfbResolution }) -> do
      let (w, h) = fromMaybe (1920, 1080) xvfbResolution
      return (proc "xvfb-run"
                    ["--auto-servernum"
                    , [i|--server-args="-screen 0 #{w}x#{h}x24"|]
                    , "java"
                    , [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                    , "-jar", seleniumPath
                    , "-port", show port
                    ])
