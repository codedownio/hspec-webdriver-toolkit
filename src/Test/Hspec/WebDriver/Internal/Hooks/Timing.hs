{-# LANGUAGE ViewPatterns #-}

module Test.Hspec.WebDriver.Internal.Hooks.Timing (
  recordTestTiming
  ) where

import Control.Concurrent
import qualified Control.Exception.Lifted as EL
import Control.Lens as L
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A
import qualified Data.ByteString.Lazy as BL
import Data.Convertible
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Stack
import System.FilePath
import qualified Test.Hspec as H
import qualified Test.Hspec.WebDriver.Internal.Hooks.Timing.TreeMap as TreeMap
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util


-- * Hooks

recordTestTiming = (H.aroundWith recordIndividualTestTiming) . (H.afterAll saveTestTiming)

-- * Implementation

recordIndividualTestTiming :: (HasCallStack) => (WdSession -> IO b) -> WdSession -> IO ()
recordIndividualTestTiming action session@(WdSession {wdTimingInfo, wdLabels}) = do
  (eitherResult, timeDiff) <- liftIO $ timeItCatchingException (action session)

  unless (null wdLabels) $ do
    let atKey (convert -> k) = A._Object . at k . non (A.object [])
    let traversal = foldl1 (.) $ atKey <$> reverse wdLabels
    timingInfo <- readMVar wdTimingInfo
    let updatedTimingInfo = set (traversal . atKey ("time" :: String)) (A.Number $ realToFrac timeDiff) timingInfo
    modifyMVar_ wdTimingInfo $ const $ return updatedTimingInfo

  whenLeft eitherResult EL.throw

saveTestTiming :: (HasCallStack) => WdSession -> IO ()
saveTestTiming (WdSession {wdTimingInfo, wdOptions=(WdOptions {runRoot})}) = do
  timingInfo <- readMVar wdTimingInfo
  BL.writeFile (runRoot </> "timing.json") (A.encode timingInfo)
  T.writeFile (runRoot </> "timing.html") (TreeMap.getHTML timingInfo)

timeItCatchingException :: (MonadIO m, MonadBaseControl IO m) => m a -> m (Either EL.SomeException a, NominalDiffTime)
timeItCatchingException action = do
  now <- liftIO getPOSIXTime
  ret <- EL.catch (Right <$> action) (\(e :: EL.SomeException) -> return $ Left e)
  endTime <- liftIO getPOSIXTime
  return (ret, endTime - now)
