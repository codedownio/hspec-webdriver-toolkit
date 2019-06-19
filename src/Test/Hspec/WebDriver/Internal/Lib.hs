{-# LANGUAGE TypeFamilies, InstanceSigs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

module Test.Hspec.WebDriver.Internal.Lib where

import Control.Concurrent.MVar
import Control.Exception
import qualified Control.Exception.Lifted as EL
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Internal.Exceptions
import Test.Hspec.WebDriver.Internal.Hooks.Logs
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W

instance Example WdExample where
  type Arg WdExample = WdSession

  evaluateExample :: (HasCallStack) => WdExample -> Params -> (ActionWith (Arg WdExample) -> IO ()) -> ProgressCallback -> IO Result

  evaluateExample (WdPending msg) _ _ _ = return $ Result "" $ Pending Nothing msg

  evaluateExample (WdExampleEveryBrowser action) _ act _ = do
    act $ \session@(WdSession {wdSessionMap}) -> do
      sessionMap <- readMVar wdSessionMap
      forM_ (M.toList sessionMap) $ \(browser, _) -> do
        runActionWithBrowser browser action session

    return $ Result "" Success

  evaluateExample (WdExample browser action) _ act _ = do
    act $ runActionWithBrowser browser action
    return $ Result "" Success


runActionWithBrowser :: Browser -> W.WD a -> WdSession -> IO a
runActionWithBrowser browser action sessionWithLabels@(WdSession {..}) = do
  -- Create new session if necessary (this can throw an exception)
  sess <- modifyMVar wdSessionMap $ \sessionMap -> case M.lookup browser sessionMap of
    Just sess -> return (sessionMap, sess)
    Nothing -> do
      sess'' <- W.mkSession wdConfig
      let sess' = sess'' { W.wdSessHistUpdate = W.unlimitedHistory }
      sess <- W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
      return (M.insert browser sess sessionMap, sess)

  -- Run the test example, handling the exception specially
  (liftIO $ try $ W.runWD sess $ do
      -- After the action, grab the updated session and save it before we return
      EL.finally action $ do
        sess' <- W.getSession
        liftIO $ modifyMVar_ wdSessionMap $ return . M.insert browser sess'
    ) >>= \case
    Left e -> liftIO $ do
      saveSessionHistoryIfConfigured sessionWithLabels

      -- Ignore the fact that there were failing browser logs, since the test failed already for other reasons
      _failingLogs <- saveBrowserLogsIfConfigured sessionWithLabels

      handleTestException sessionWithLabels e
      throw e -- Rethrow for the test framework to handle
    Right x -> do
      liftIO $ saveSessionHistoryIfConfigured sessionWithLabels

      failingLogs <- saveBrowserLogsIfConfigured sessionWithLabels
      case failingLogs of
        [] -> return x
        xs -> do
          -- Throw an exception because of the failing browser logs
          let e = InvalidLogsException xs
          handleTestException sessionWithLabels (SomeException e)
          throw e

runWithBrowser :: Browser -> W.WD () -> WdExample
runWithBrowser = WdExample

runWithBrowser' :: Browser -> W.WD () -> WdSession -> IO ()
runWithBrowser' browser action session = do
  runActionWithBrowser browser action session

runEveryBrowser :: W.WD () -> WdExample
runEveryBrowser = WdExampleEveryBrowser

runEveryBrowser' :: W.WD () -> WdSession -> IO ()
runEveryBrowser' action session@(WdSession {wdSessionMap}) = do
  sessionMap <- readMVar wdSessionMap
  forM_ (M.toList sessionMap) $ \(browser, _) -> do
    runActionWithBrowser browser action session

executeWithBrowser :: Browser -> WdSession -> W.WD a -> W.WD a
executeWithBrowser browser session action = do
  liftIO $ runActionWithBrowser browser action session

closeSession :: Browser -> WdSession -> IO ()
closeSession browser (WdSession {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    whenJust (M.lookup browser sessionMap) $ \sess ->
      W.runWD sess W.closeSession
    return $ M.delete browser sessionMap

closeAllSessionsExcept :: [Browser] -> WdSession -> IO ()
closeAllSessionsExcept toKeep (WdSession {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    forM_ (M.toList sessionMap) $ \(name, sess) -> unless (name `elem` toKeep) $
      catch (W.runWD sess W.closeSession)
            (\(e :: SomeException) -> putStrLn [i|Failed to destroy session '#{name}': '#{e}'|])
    return $ M.fromList [(b, s) | (b, s) <- M.toList sessionMap, b `elem` toKeep]

closeAllSessions :: WdSession -> IO ()
closeAllSessions = closeAllSessionsExcept []
