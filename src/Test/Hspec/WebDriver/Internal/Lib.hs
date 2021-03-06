{-# LANGUAGE TypeFamilies, InstanceSigs, ScopedTypeVariables, QuasiQuotes, ViewPatterns #-}

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
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W

instance HasWdSession a => Example (WdExample a) where
  type Arg (WdExample a) = a

  evaluateExample :: (HasCallStack) => WdExample a -> Params -> (ActionWith (Arg (WdExample a)) -> IO ()) -> ProgressCallback -> IO Result

  evaluateExample (WdPending msg) _ _ _ = return $ Result "" $ Pending Nothing msg

  evaluateExample (WdExampleEveryBrowser action) _ act _ = do
    act $ \(getWdSession -> session@(WdSession {wdSessionMap})) -> do
      sessionMap <- readMVar wdSessionMap
      forM_ (M.toList sessionMap) $ \(browser, _) -> do
        runActionWithBrowser browser action session

    return $ Result "" Success

  evaluateExample (WdExample browser action) _ act _ = do
    act $ \(getWdSession -> session) -> runActionWithBrowser browser action session
    return $ Result "" Success


runActionWithBrowser :: (HasCallStack) => Browser -> W.WD a -> WdSession -> IO a
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
      handleTestException sessionWithLabels e
      throw e -- Rethrow for the test framework to handle
    Right x -> return x


runWithBrowser :: (HasCallStack, HasWdSession a) => Browser -> W.WD () -> WdExample a
runWithBrowser = WdExample

runWithBrowser' :: (HasCallStack, HasWdSession a) => Browser -> W.WD () -> a -> IO ()
runWithBrowser' browser action hasSession = do
  runActionWithBrowser browser action (getWdSession hasSession)

runEveryBrowser :: (HasCallStack, HasWdSession a) => W.WD () -> WdExample a
runEveryBrowser = WdExampleEveryBrowser

runEveryBrowser' :: (HasCallStack, HasWdSession a) => W.WD () -> a -> IO ()
runEveryBrowser' action (getWdSession -> session@(WdSession {wdSessionMap})) = do
  sessionMap <- readMVar wdSessionMap
  forM_ (M.toList sessionMap) $ \(browser, _) -> do
    runActionWithBrowser browser action session

executeWithBrowser :: (HasCallStack) => Browser -> WdSession -> W.WD a -> W.WD a
executeWithBrowser browser session action = do
  liftIO $ runActionWithBrowser browser action session

closeSession :: (HasCallStack) => Browser -> WdSession -> IO ()
closeSession browser (WdSession {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    whenJust (M.lookup browser sessionMap) $ \sess ->
      W.runWD sess W.closeSession
    return $ M.delete browser sessionMap

closeAllSessionsExcept :: (HasCallStack) => [Browser] -> WdSession -> IO ()
closeAllSessionsExcept toKeep (WdSession {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    forM_ (M.toList sessionMap) $ \(name, sess) -> unless (name `elem` toKeep) $
      catch (W.runWD sess W.closeSession)
            (\(e :: SomeException) -> putStrLn [i|Failed to destroy session '#{name}': '#{e}'|])
    return $ M.fromList [(b, s) | (b, s) <- M.toList sessionMap, b `elem` toKeep]

closeAllSessions :: (HasCallStack) => WdSession -> IO ()
closeAllSessions = closeAllSessionsExcept []
