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

  evaluateExample (WdExampleEveryBrowser action) _ act _ = do
    act $ \session@(WdSession {wdSessionMap}) -> do
      resultVar <- newEmptyMVar
      sessionMap <- readMVar wdSessionMap
      forM_ sessionMap $ \(browser, _) -> runActionWithBrowser resultVar browser action session
      void $ takeMVar resultVar

    return $ Result "" Success

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

runWithBrowser' :: Browser -> W.WD () -> WdSession -> IO WdSession
runWithBrowser' browser action session = do
  runWithBrowser'' browser action session
  return session

runWithBrowser'' :: Browser -> W.WD () -> WdSession -> IO ()
runWithBrowser'' browser action session = do
  resultVar <- newEmptyMVar
  runActionWithBrowser resultVar browser action session

runEveryBrowser :: W.WD () -> WdExample
runEveryBrowser = WdExampleEveryBrowser

runEveryBrowser' :: W.WD () -> WdSession -> IO ()
runEveryBrowser' action session@(WdSession {wdSessionMap}) = do
  resultVar <- newEmptyMVar
  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(browser, sess) -> do
    runActionWithBrowser resultVar browser action session

executeWithBrowser :: Browser -> WdSession -> W.WD () -> W.WD ()
executeWithBrowser browser session action = do
  resultVar <- liftIO newEmptyMVar
  liftIO $ runActionWithBrowser resultVar browser action session

closeSession :: Browser -> WdSession -> IO ()
closeSession browser (WdSession {wdSessionMap}) = do
  sessionMap <- readMVar wdSessionMap
  whenJust (L.lookup browser sessionMap) $ \sess ->
    W.runWD sess W.closeSession

closeAllSessionsExcept :: [Browser] -> WdSession -> IO ()
closeAllSessionsExcept toKeep (WdSession {wdSessionMap}) = do
  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(name, sess) -> unless (name `elem` toKeep) $
    catch (W.runWD sess W.closeSession)
          (\(e :: SomeException) -> putStrLn [i|Failed to destroy session '#{name}': '#{e}'|])

closeAllSessions :: WdSession -> IO ()
closeAllSessions = closeAllSessionsExcept []
