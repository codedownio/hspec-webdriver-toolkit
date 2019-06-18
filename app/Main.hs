{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables, ViewPatterns #-}
module Main where

import Data.Default
import Data.String.Interpolate.IsString
import System.Directory
import System.FilePath
import Test.Hspec (hspec)
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Toolkit
import Test.WebDriver.Commands


tests :: SpecType
tests = describe "Basic widget tests" $ do
  describe "Basic editing" $ do
    it "does the first thing" $ runWithBrowser "browser1" $ do
      2 `shouldBe` 2

    it "does the second thing" $ runWithBrowser "browser1" $ do
      3 `shouldBe` 3

    it "starts a browser" $ runWithBrowser "browser1" $ do
      openPage "http://www.google.com"

      elem <- findElem (ByCSS "input[title=Search]")
      -- liftIO $ putStrLn [i|Found elem: #{elem}|]
      2 `shouldBe` 2
      -- click elem
      -- sendKeys "hello world" elem

    it "starts another browser" $ runWithBrowser "browser2" $ do
      openPage "http://www.yahoo.com"

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
        , runMode = Normal
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
                                  . recordEntireVideo (def {hideMouseWhenRecording = True})
                                 ) tests
