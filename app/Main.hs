{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables, ViewPatterns #-}
module Main where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Default
import Data.String.Interpolate.IsString
import System.Directory
import System.FilePath
import Test.Hspec (hspec, after)
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Toolkit
import Test.WebDriver.Commands

printResultsDir session = do
  putStrLn [i|Got wdLabels in after hook: #{getResultsDir session}|]

tests :: SpecType
tests = describe "Basic tests" $ do
  it "does the first thing" $ runWithBrowser "browser1" $ do
    2 `shouldBe` 2

  it "does the second thing" $ \session -> do
    putStrLn [i|Got results dir in second thing: #{getResultsDir session}|]
    -- 3 `shouldBe` 3

  it "starts a browser" $ runWithBrowser "browser1" $ do
    openPage "http://www.google.com"

    elem <- findElem (ByCSS "input[title=Search]")
    -- liftIO $ putStrLn [i|Found elem: #{elem}|]
    liftIO $ threadDelay 3000000
    2 `shouldBe` 3
    -- click elem
    -- sendKeys "hello world" elem

  it "starts another browser" $ runWithBrowser "browser2" $ do
    openPage "http://www.xkcd.com"

main :: IO ()
main = do
  let testRoot = "/tmp/testroot"
  let toolsRoot = testRoot </> "test_tools"
  let runsRoot = testRoot </> "test_runs"
  createDirectoryIfMissing True toolsRoot
  runRoot <- getTestFolder runsRoot
  createDirectoryIfMissing True runRoot

  putStrLn [i|\n********** Test root: #{testRoot} **********|]

  let wdOptions = (defaultWdOptions toolsRoot runRoot) {
        capabilities = chromeCapabilities
        , runMode = RunInXvfb def
        }

  -- hspec $ runWebDriver wdOptions (
  --   screenshotBeforeAndAfterTest .
  --   recordEntireVideo .
  --   recordIndividualVideos .
  --   saveWebDriverLogs .
  --   saveBrowserLogs
  --   ) tests
  hspec $ runWebDriver wdOptions (recordTestTiming
                                  . saveWebDriverLogs
                                  . recordEntireVideo def
                                  . recordErrorVideos def
                                  . recordIndividualVideos def
                                 ) tests
