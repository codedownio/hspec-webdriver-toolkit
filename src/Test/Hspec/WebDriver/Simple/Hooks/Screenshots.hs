
{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Simple.Hooks.Screenshots (
  screenshotBeforeTest
  , screenshotAfterTest
  , screenshotBeforeAndAfterTest

  , saveScreenshots
  ) where

import Control.Concurrent
import Control.Exception.Lifted as EL
import Control.Monad
import Control.Retry
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import qualified Network.Socket as N
import System.Directory
import System.FilePath
import System.IO
import System.Random (randomRIO)
import Test.Hspec
import Test.Hspec.WebDriver.Simple.Types
import Test.Hspec.WebDriver.Simple.Util
import Test.WebDriver
import Text.Printf

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

saveScreenshots :: (HasCallStack) => T.Text -> WdSessionWithLabels -> IO ()
saveScreenshots screenshotName sessionWithLabels@(WdSessionWithLabels {..}) = do
  let resultsDir = getResultsDir sessionWithLabels
  createDirectoryIfMissing True resultsDir

  -- For every session, and for every window, try to get a screenshot for the results dir
  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(browser, sess) -> runWD sess $ do
    ws <- windows
    forM_ ws $ \w@(WindowHandle windowText) -> EL.handle swallowNoSuchWindowException $ do
      focusWindow w
      saveScreenshot $ resultsDir </> [i|#{browser}_#{windowText}_#{screenshotName}.png|]

swallowNoSuchWindowException (FailedCommand NoSuchWindow _) = return ()
swallowNoSuchWindowException e = EL.throw e
