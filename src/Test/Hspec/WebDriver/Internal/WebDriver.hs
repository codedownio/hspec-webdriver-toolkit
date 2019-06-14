{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, LambdaCase, Rank2Types #-}

module Test.Hspec.WebDriver.Internal.WebDriver (
  startWebDriver
  , stopWebDriver
  ) where

import Control.Concurrent
import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import Data.Default
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Network.Socket (PortNumber)
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Test.Hspec.WebDriver.Internal.Binaries
import Test.Hspec.WebDriver.Internal.Binaries.Util
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
  wdCreateProcess <- getWebdriverCreateProcess toolsRoot port >>= \case
    Left err -> error [i|Failed to create webdriver process: '#{err}'|]
    Right x -> return x

  -- Open output handles
  let logsDir = runRoot </> "selenium_logs"
  createDirectoryIfMissing True logsDir
  hout <- openFile (logsDir </> seleniumOutFileName) AppendMode
  herr <- openFile (logsDir </> seleniumErrFileName) AppendMode

  -- Start the process and wait for it to be ready
  (_, _, _, p) <- createProcess $ wdCreateProcess {
    std_in = Inherit
    , std_out = UseHandle hout
    , std_err = UseHandle herr
    }
  withFile (logsDir </> seleniumErrFileName) ReadMode $
    flip waitForMessage "Selenium Server is up and running"

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

getWebdriverCreateProcess :: FilePath -> PortNumber -> IO (Either T.Text CreateProcess)
getWebdriverCreateProcess toolsDir port = runExceptT $ do
  chromeDriverPath <- ExceptT $ downloadChromeDriverIfNecessary toolsDir
  seleniumPath <- ExceptT $ downloadSeleniumIfNecessary toolsDir
  return (proc "java" [
             [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
               ,"-jar", seleniumPath
               , "-port", show port
               ])
