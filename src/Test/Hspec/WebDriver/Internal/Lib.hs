{-# LANGUAGE TypeFamilies, InstanceSigs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

module Test.Hspec.WebDriver.Internal.Lib where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Either
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Internal.Exceptions
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W

instance Example WdExample where
  type Arg WdExample = WdSession

  evaluateExample :: (HasCallStack) => WdExample -> Params -> (ActionWith (Arg WdExample) -> IO ()) -> ProgressCallback -> IO Result
  evaluateExample (WdPending msg) _ _ _ = return $ Result "" $ Pending Nothing msg
  evaluateExample (WdExample browser action) _ act _ = do
    resultVar <- newEmptyMVar
    act $ runActionWithBrowser resultVar browser action
    takeMVar resultVar

-- instance (W.WebDriver wd) => Example (wd ()) where
--   type Arg (wd ()) = ()

--   evaluateExample :: (HasCallStack) => wd () -> Params -> (ActionWith (Arg (wd ())) -> IO ()) -> ProgressCallback -> IO Result
--   evaluateExample action _ act _ = do
--     resultVar <- newEmptyMVar
--     act $ runActionWithBrowser resultVar "browser1" action
--     takeMVar resultVar


runActionWithBrowser :: MVar Result -> Browser -> W.WD () -> WdSession -> IO ()
runActionWithBrowser resultVar browser action sessionWithLabels@(WdSession {..}) = do
  eitherResult :: Either Result () <- runExceptT $ do

    -- Create new session if necessary
    sess <- ExceptT $ modifyMVar wdSessionMap $ \sessionMap -> case L.lookup browser sessionMap of
      Just sess -> return (sessionMap, Right sess)
      Nothing -> handle (\(e :: SomeException) -> return (sessionMap, Left $ Result "Exception while creating WebDriver session" (Failure Nothing (Error Nothing e))))
                        (do
                            sess' <- W.mkSession wdConfig
                            sess <- W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
                            return ((browser, sess):sessionMap, Right sess)
                        )

    -- Run the test example, handling the exception specially
    (liftIO $ try $ W.runWD sess action) >>= \case
      Left e -> liftIO $ do
        saveSessionHistoryIfConfigured sessionWithLabels
        handleTestException sessionWithLabels e
        throw e -- Rethrow for the test framework to handle
      Right () -> do
        liftIO $ saveSessionHistoryIfConfigured sessionWithLabels
        return ()

  putMVar resultVar (fromLeft (Result "" Success) eitherResult)

runWithBrowser :: Browser -> W.WD () -> WdExample
runWithBrowser = WdExample

runEveryBrowser :: Browser -> W.WD () -> WdExample
runEveryBrowser = WdExample

runWithBrowser' :: Browser -> W.WD () -> WdSession -> IO WdSession
runWithBrowser' browser action session = do
  resultVar <- newEmptyMVar
  runActionWithBrowser resultVar browser action session
  return session

closeSession :: Browser -> WdSession -> IO ()
closeSession browser (WdSession {wdSessionMap}) = do
  sessionMap <- readMVar wdSessionMap
  whenJust (L.lookup browser sessionMap) $ \sess ->
    W.runWD sess W.closeSession

closeAllSessions :: WdSession -> IO ()
closeAllSessions (WdSession {wdSessionMap}) = do
  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(name, sess) -> do
    catch (W.runWD sess W.closeSession)
          (\(e :: SomeException) -> putStrLn [i|Failed to destroy session '#{name}': '#{e}'|])
