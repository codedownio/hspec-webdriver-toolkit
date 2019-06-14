{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Internal.Hooks.Screenshots (
  screenshotBeforeTest
  , screenshotAfterTest
  , screenshotBeforeAndAfterTest

  , saveScreenshots
  ) where

import Control.Concurrent
import Control.Monad
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import Test.WebDriver

-- * Hooks

-- | Take a screenshot of the browser(s) before running each test
screenshotBeforeTest :: Hook
screenshotBeforeTest = beforeWith (\x -> saveScreenshots "before" x >> return x)

-- | Take a screenshot of the browser(s) after running each test
screenshotAfterTest :: Hook
screenshotAfterTest = after (saveScreenshots "after")

-- | Take a screenshot of the browser(s) before and after running each test
screenshotBeforeAndAfterTest :: Hook
screenshotBeforeAndAfterTest = screenshotBeforeTest . screenshotAfterTest

-- * Implementation

saveScreenshots :: (HasCallStack) => T.Text -> WdSession -> IO ()
saveScreenshots screenshotName sessionWithLabels@(WdSession {..}) = do
  let resultsDir = getResultsDir sessionWithLabels
  createDirectoryIfMissing True resultsDir

  -- For every session, and for every window, try to get a screenshot for the results dir
  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(browser, sess) -> runWD sess $
    saveScreenshot $ resultsDir </> [i|#{browser}_#{screenshotName}.png|]
