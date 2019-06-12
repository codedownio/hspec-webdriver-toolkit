{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns #-}

module Test.Hspec.WebDriver.Simple.Binaries where

import Control.Concurrent
import qualified Control.Exception.Lifted as E
import Control.Monad
import Data.Default
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import Network.Socket (PortNumber)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import qualified Test.Hspec as H
import Test.Hspec.WebDriver.Simple.Ports
import Test.Hspec.WebDriver.Simple.Types
import Test.Hspec.WebDriver.Simple.Util
import qualified Test.WebDriver.Config as W

seleniumErrFileName, seleniumOutFileName :: String
seleniumErrFileName = "selenium_stderr.log"
seleniumOutFileName = "selenium_stdout.log"

downloadChromeDriver :: FilePath -> IO ()
downloadChromeDriver folder = void $ do
  path <- canonicalizePath folder
  putStrLn [i|Downloading chromedriver to #{path}|]
  createDirectoryIfMissing True path
  void $ readCreateProcess (shell [i|wget -nc -O - https://chromedriver.storage.googleapis.com/2.45/chromedriver_linux64.zip | gunzip - > #{path}/chromedriver|]) ""
  readCreateProcess (shell [i|chmod u+x #{path}/chromedriver|]) ""

downloadSelenium :: FilePath -> IO ()
downloadSelenium folder = void $ do
  path <- canonicalizePath folder
  putStrLn [i|Downloading selenium-server.jar to #{path}|]
  createDirectoryIfMissing True path
  readCreateProcess (shell [i|curl https://selenium-release.storage.googleapis.com/3.9/selenium-server-standalone-3.9.1.jar -o #{path}/selenium-server.jar|]) ""

ensureSeleniumBinariesPresent :: FilePath -> IO ()
ensureSeleniumBinariesPresent folder = do
  path <- canonicalizePath folder
  doesFileExist [i|#{path}/chromedriver|] >>= flip unless (downloadChromeDriver folder)
  doesFileExist [i|#{path}/selenium-server.jar|] >>= flip unless (downloadSelenium folder)

webdriverCreateProcess :: FilePath -> PortNumber -> CreateProcess
webdriverCreateProcess toolsDir port =
  (proc "java" [
      [i|-Dwebdriver.chrome.driver=#{toolsDir}/chromedriver|]

      ,"-jar"
      , [i|#{toolsDir}/selenium-server.jar|]

      , "-port"
      , show port
      ])

-- | Spin up a Selenium WebDriver and perform a callback while it's running.
-- Shut it down afterwards.
withWebDriver :: WdOptions -> (W.WDConfig -> Hooks -> IO a) -> IO a
withWebDriver (WdOptions {testRoot, toolsDir=maybeToolsDir, runRoot}) action = do

  createDirectoryIfMissing True testRoot

  port <- findFreePortOrException

  putStrLn [i|Starting selenium server on port: #{port}|]

  let toolsDir = fromMaybe (testRoot </> "test_tools") maybeToolsDir
  createDirectoryIfMissing True toolsDir
  ensureSeleniumBinariesPresent toolsDir

  chromeDataDir <- createTempDirectory runRoot "chromedata"

  let logsDir = runRoot </> "selenium_logs"
  createDirectoryIfMissing True logsDir
  let outFilePath = logsDir </> seleniumOutFileName
  let errFilePath =  logsDir </> seleniumErrFileName

  -- Selenium log collection is disabled for now since it's rarely useful
  (withFile outFilePath AppendMode) $ \hout ->
    (withFile errFilePath AppendMode) $ \herr -> do

      let hooks = H.after $ \sessionWithLabels -> do
            let resultsDir = getResultsDir sessionWithLabels
            createDirectoryIfMissing True resultsDir
            moveAndTruncate outFilePath (resultsDir </> seleniumOutFileName)
            moveAndTruncate errFilePath (resultsDir </> seleniumErrFileName)

      E.bracket (do
                     p@(_, _, _, _) <- createProcess $ (webdriverCreateProcess toolsDir port) {
                       std_in = Inherit
                       , std_out = UseHandle hout
                       , std_err = UseHandle herr
                       }

                     withFile errFilePath ReadMode $ flip waitForMessage "Selenium Server is up and running"

                     return p
                )
                (\(_, _, _, h) -> terminateProcess h >> waitForProcess h)
                (const $ action (def { W.wdPort = fromIntegral port }) hooks)


waitForMessage :: Handle -> String -> IO ()
waitForMessage h msg = waitForMessage' h (reverse msg) ""
  where
    waitForMessage' :: Handle -> String -> String -> IO ()
    waitForMessage' h msg partial = do
      c <- getCharFile h
      let newMsg = c : partial
      if msg `L.isInfixOf` newMsg then return ()
      else waitForMessage' h msg newMsg

    getCharFile h = E.catch (hGetChar h) (\(_ :: IOError) -> (threadDelay 10000) >> (getCharFile h))
