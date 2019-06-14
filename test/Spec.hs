{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables, ViewPatterns #-}

import Data.Default
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Hspec.WebDriver.Toolkit hiding (shouldBe)
import Test.WebDriver.Commands

tests :: Spec
tests = describe "Tookit tests" $ do
  describe "Individual hook tests" $ do
    it "can take screenshots before and after tests" $ do
      cwd <- getCurrentDirectory
      let toolsRoot = cwd </> "test_tools"
      let runsRoot = cwd </> "test_runs"
      createDirectoryIfMissing True toolsRoot
      runRoot <- getTestFolder runsRoot
      createDirectoryIfMissing True runRoot

      let wdOptions = def { toolsRoot = toolsRoot
                          , runRoot = runRoot
                          , capabilities = headlessChromeCapabilities
                          -- , capabilities = chromeCapabilities
                          }

      hspec $ runWebDriver wdOptions screenshotBeforeAndAfterTest $ do
        it "step_one" $ runWithBrowser "browser1" $ do
          openPage "http://www.google.com"

        it "step_two" $ runWithBrowser "browser1" $ do
          openPage "http://www.xkcd.com"

      doesFileExist (runRoot </> "results" </> "step_one" </> "browser1_after.png") >>= (`shouldBe` True)
      doesFileExist (runRoot </> "results" </> "step_two" </> "browser1_before.png") >>= (`shouldBe` True)
      doesFileExist (runRoot </> "results" </> "step_two" </> "browser1_after.png") >>= (`shouldBe` True)


-- TODO: add a test for the fact that forward slashes in test labels result in extra directories

main :: IO ()
main = hspec tests
