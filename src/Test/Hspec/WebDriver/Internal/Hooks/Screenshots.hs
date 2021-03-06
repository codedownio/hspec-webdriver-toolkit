{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}

module Test.Hspec.WebDriver.Internal.Hooks.Screenshots (
  screenshotBeforeTest
  , screenshotAfterTest
  , screenshotBeforeAndAfterTest

  , saveScreenshots
  ) where

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import Network.HTTP.Client
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
  forM_ (M.toList sessionMap) $ \(browser, sess) -> runWD sess $
    handle (\(e :: HttpException) -> case e of
               (HttpExceptionRequest _ content) -> liftIO $ putStrLn [i|HttpException when trying to take a screenshot: '#{content}'|]
               e -> liftIO $ putStrLn [i|HttpException when trying to take a screenshot: '#{e}'|])
           (saveScreenshot $ resultsDir </> [i|#{browser}_#{screenshotName}.png|])
