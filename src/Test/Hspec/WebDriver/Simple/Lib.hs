{-# LANGUAGE TypeFamilies, InstanceSigs, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}

module Test.Hspec.WebDriver.Simple.Lib where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import Data.Either
import qualified Data.List as L
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Simple.Exceptions
import Test.Hspec.WebDriver.Simple.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W

instance Example WdExample where
  type Arg WdExample = WdSessionWithLabels

  evaluateExample :: (HasCallStack) => WdExample -> Params -> (ActionWith (Arg WdExample) -> IO ()) -> ProgressCallback -> IO Result
  evaluateExample (WdPending msg) _ _ _ = return $ Result "" $ Pending Nothing msg
  evaluateExample (WdExample browser action) _ act _ = do
    resultVar <- newEmptyMVar

    act $ \sessionWithLabels@(WdSessionWithLabels {..}) -> do
      eitherResult :: Either Result () <- runExceptT $ do

        -- Create new session if necessary
        sess <- ExceptT $ modifyMVar wdSessionMap $ \sessionMap -> case L.lookup browser sessionMap of
          Just sess -> return (sessionMap, Right sess)
          Nothing -> handle (\(e :: SomeException) -> return $ (sessionMap, Left $ Result "Exception while creating WebDriver session" (Failure Nothing (Error Nothing e))))
                            (do
                                sess' <- W.mkSession wdConfig
                                sess <- W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
                                return ((browser, sess):sessionMap, Right sess)
                            )

        -- Run the test example, handling the exception specially
        (liftIO (try $ W.runWD sess action)) >>= \case
          Left e -> liftIO $ do
            handleTestException sessionWithLabels e
            throw e -- Rethrow for the test framework to handle
          Right () -> return ()

      putMVar resultVar (fromLeft (Result "" Success) eitherResult)

    takeMVar resultVar


runWithBrowser :: Browser -> W.WD () -> WdExample
runWithBrowser browser action = WdExample browser action

closeAllSessions :: WdSessionWithLabels -> IO ()
closeAllSessions (WdSessionWithLabels {wdSessionMap}) = do
  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(name, sess) -> do
    putStrLn [i|Closing session '#{name}'|]
    catch (W.runWD sess W.closeSession)
          (\(e :: SomeException) -> putStrLn [i|Failed to destroy session '#{name}': #{e}|])

makeInitialSessionWithLabels wdOptions baseConfig caps = do
  let wdConfig = baseConfig { W.wdCapabilities = caps }
  WdSessionWithLabels <$> (pure []) <*> (pure wdOptions) <*> (newMVar []) <*> (newMVar 0) <*> (newMVar Nothing) <*> (pure wdConfig)
