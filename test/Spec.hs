{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables, ViewPatterns #-}

import Data.String.Interpolate.IsString
import System.Directory
import System.FilePath
import Test.Hspec as H
import Test.Hspec.Core.Runner
import Test.Hspec.WebDriver.Toolkit hiding (shouldBe)
import Test.WebDriver.Commands
import Test.WebDriver.JSON

tests :: Spec
tests = describe "Tookit tests" $ do
  describe "Individual hook tests" $ do
    it "can fail the tests on severe browser logs" $ do
      wdOptions <- getTestWdOptions
      let hspecOutputFile = (runRoot wdOptions) </> "hspec_output.txt"
      let config = hspecConfigFileOutput hspecOutputFile

      (Summary {..}) <- hspecWithResult config $ runWebDriver wdOptions failOnSevereBrowserLogs $ do
        it "has_console_error" $ runWithBrowser "browser1" $ do
          openPage "http://www.google.com"
          ignoreReturn $ executeJS [] [i|console.error("Oh no this shouldn't happen")|]

        it "has_no_console_error" $ runWithBrowser "browser1" $ do
          openPage "http://www.google.com"

      summaryExamples `shouldBe` 2
      summaryFailures `shouldBe` 1

      readFile hspecOutputFile >>= (`H.shouldContain` "uncaught exception: InvalidLogsException")

    it "can take screenshots before and after tests" $ do
      wdOptions <- getTestWdOptions
      let config = hspecConfigFileOutput $ (runRoot wdOptions) </> "hspec_output.txt"

      (Summary {..}) <- hspecWithResult config $ runWebDriver wdOptions screenshotBeforeAndAfterTest $ do
        it "step_one" $ runWithBrowser "browser1" $ do
          openPage "http://www.google.com"

        it "step_two" $ runWithBrowser "browser1" $ do
          openPage "http://www.xkcd.com"

      summaryFailures `shouldBe` 0

      doesFileExist (runRoot wdOptions </> "results" </> "step_one" </> "browser1_after.png") >>= (`shouldBe` True)
      doesFileExist (runRoot wdOptions </> "results" </> "step_two" </> "browser1_before.png") >>= (`shouldBe` True)
      doesFileExist (runRoot wdOptions </> "results" </> "step_two" </> "browser1_after.png") >>= (`shouldBe` True)


-- TODO: add a test for the fact that forward slashes in test labels result in extra directories

getTestWdOptions :: IO WdOptions
getTestWdOptions = do
  cwd <- getCurrentDirectory
  let toolsRoot = cwd </> "test_tools"
  let runsRoot = cwd </> "test_runs"
  createDirectoryIfMissing True toolsRoot
  runRoot <- getTestFolder runsRoot
  createDirectoryIfMissing True runRoot
  return $ (defaultWdOptions toolsRoot runRoot) { capabilities = headlessChromeCapabilities }


hspecConfigFileOutput :: FilePath -> Config
hspecConfigFileOutput path = defaultConfig { configFailureReport = Just "hspec_failures.txt"
                                           , configOutputFile = Right path }

main :: IO ()
main = hspec tests
