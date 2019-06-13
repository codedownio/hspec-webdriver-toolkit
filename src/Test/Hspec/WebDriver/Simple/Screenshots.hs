
{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Simple.Screenshots where

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

screenshotBeforeHook = beforeWith (\x -> saveScreenshots "before" x >> return x)
screenshotAfterHook = after (saveScreenshots "after")
screenshotHooks = screenshotBeforeHook . screenshotAfterHook

-- * Implementation


saveScreenshots :: (HasCallStack) => T.Text -> WdSessionWithLabels -> IO ()
saveScreenshots screenshotName sessionWithLabels@(WdSessionWithLabels {wdSession=(WdSession {..}), ..}) = do
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
