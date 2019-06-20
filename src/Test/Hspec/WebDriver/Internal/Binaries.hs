{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, Rank2Types #-}

module Test.Hspec.WebDriver.Internal.Binaries (
  downloadSeleniumIfNecessary
  , downloadChromeDriverIfNecessary
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.List as L
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec.WebDriver.Internal.Binaries.Util
import Test.Hspec.WebDriver.Internal.Util

downloadSeleniumIfNecessary :: FilePath -> IO (Either T.Text FilePath)
downloadSeleniumIfNecessary toolsDir = leftOnException' $ do
  let seleniumPath = [i|#{toolsDir}/selenium-server.jar|]
  liftIO (doesFileExist seleniumPath >>= flip unless (downloadSelenium seleniumPath))
  return seleniumPath

downloadSelenium :: FilePath -> IO ()
downloadSelenium seleniumPath = void $ do
  putStrLn [i|Downloading selenium-server.jar to #{seleniumPath}|]
  createDirectoryIfMissing True (takeDirectory seleniumPath)
  readCreateProcess (shell [i|curl https://selenium-release.storage.googleapis.com/3.141/selenium-server-standalone-3.141.59.jar -o #{seleniumPath}|]) ""

downloadChromeDriverIfNecessary :: FilePath -> IO (Either T.Text FilePath)
downloadChromeDriverIfNecessary toolsDir = runExceptT $ do
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

  return chromeDriverPath

downloadAndUnzipToPath :: T.Text -> FilePath -> IO (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  putStrLn [i|Downloading #{downloadPath} to #{localPath}|]
  createDirectoryIfMissing True (takeDirectory localPath)
  void $ readCreateProcess (shell [i|wget -nc -O - #{downloadPath} | gunzip - > #{localPath}|]) ""
  void $ readCreateProcess (shell [i|chmod u+x #{localPath}|]) ""
