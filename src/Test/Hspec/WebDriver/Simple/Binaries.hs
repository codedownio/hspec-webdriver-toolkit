{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, LambdaCase, Rank2Types #-}

module Test.Hspec.WebDriver.Simple.Binaries where

import Control.Concurrent
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Network.Socket (PortNumber)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import qualified System.Info as SI
import System.Process
import qualified Test.Hspec as H
import Test.Hspec.WebDriver.Simple.Binaries.Util
import Test.Hspec.WebDriver.Simple.Ports
import Test.Hspec.WebDriver.Simple.Types
import Test.Hspec.WebDriver.Simple.Util
import qualified Test.WebDriver.Config as W

downloadSelenium :: FilePath -> IO ()
downloadSelenium folder = void $ do
  path <- canonicalizePath folder
  putStrLn [i|Downloading selenium-server.jar to #{path}|]
  createDirectoryIfMissing True path
  readCreateProcess (shell [i|curl https://selenium-release.storage.googleapis.com/3.9/selenium-server-standalone-3.9.1.jar -o #{path}/selenium-server.jar|]) ""

getWebdriverCreateProcess :: FilePath -> PortNumber -> IO (Either T.Text CreateProcess)
getWebdriverCreateProcess toolsDir port = runExceptT $ do
  chromeMajorVersion <- ExceptT detectChromeMajorVersion

  downloadPath <- case L.lookup chromeMajorVersion chromeDriverPaths of
    Nothing -> throwE [i|Couldn't figure out which chromedriver to download for Chrome '#{chromeMajorVersion}'|]
    Just platformToPath -> case L.lookup detectPlatform platformToPath of
      Nothing -> throwE [i|Couldn't find chrome driver for platform '#{detectPlatform}'|]
      Just path -> return path

  let executableName = case detectPlatform of
        Windows -> "chromedriver.exe"
        _ -> "chromedriver"

  let chromeDriverPath = [i|#{toolsDir}/chromedrivers/#{chromeMajorVersion}/#{executableName}|]
  (liftIO $ doesFileExist chromeDriverPath) >>= flip unless (ExceptT $ downloadAndUnzipToPath downloadPath chromeDriverPath)

  -- Download selenium
  liftIO $ (doesFileExist [i|#{toolsDir}/selenium-server.jar|] >>= flip unless (downloadSelenium toolsDir))

  return (proc "java" [
             [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]

               ,"-jar", [i|#{toolsDir}/selenium-server.jar|]

               , "-port", show port
               ])

downloadAndUnzipToPath :: T.Text -> FilePath -> IO (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  putStrLn [i|Downloading #{downloadPath} to #{localPath}|]
  createDirectoryIfMissing True (takeDirectory localPath)
  void $ readCreateProcess (shell [i|wget -nc -O - #{downloadPath} | gunzip - > #{localPath}|]) ""
  void $ readCreateProcess (shell [i|chmod u+x #{localPath}|]) ""
