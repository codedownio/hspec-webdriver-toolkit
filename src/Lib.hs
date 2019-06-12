{-# LANGUAGE TypeFamilies, InstanceSigs, RecordWildCards, ScopedTypeVariables #-}

module Lib where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Default
import qualified Data.List as L
import GHC.Stack
import Test.Hspec.Core.Spec
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W

type Browser = String

newtype WdOptions = WdOptions {
  -- Whether to skip the rest of the tests once one fails
  skipRemainingTestsAfterFailure :: Bool
  }

instance Default WdOptions where
  def = WdOptions True

data WdSession = WdSession { wdOptions :: WdOptions
                           , wdSessionMap :: MVar [(Browser, W.WDSession)]
                           , wdConfig :: W.WDConfig }

data WdSessionWithLabels = WdSessionWithLabels { wdLabels :: [String]
                                               , wdSession :: WdSession }

data WdExample = WdExample { wdBrowser :: Browser
                           , wdAction :: W.WD () }
               | WdPending { wdPendingMsg :: Maybe String }

instance Example WdExample where
  type Arg WdExample = WdSessionWithLabels

  evaluateExample :: (HasCallStack) => WdExample -> Params -> (ActionWith (Arg WdExample) -> IO ()) -> ProgressCallback -> IO Result
  evaluateExample (WdPending msg) _ _ _ = return $ Result "" $ Pending Nothing msg
  evaluateExample (WdExample browser action) _ act _ = do
    resultVar <- newEmptyMVar

    act $ \(WdSessionWithLabels {wdSession=(WdSession {..}), ..}) -> do
      eitherResult :: Either Result () <- runExceptT $ do

        -- Create new session if necessary
        sess <- ExceptT $ modifyMVar wdSessionMap $ \sessionMap -> case L.lookup browser sessionMap of
          Just sess -> return (sessionMap, Right sess)
          Nothing -> handle (\(e :: SomeException) -> return $ (sessionMap, Left $ Result "Exception while creating WebDriver session" (Failure Nothing (Error Nothing e))))
                            (do
                                sess <- W.mkSession wdConfig
                                W.runWD sess $ W.createSession $ W.wdCapabilities wdConfig
                                return ((browser, sess):sessionMap, Right sess)
                            )

        liftIO $ W.runWD sess action

      let result = case eitherResult of
            Left r -> r
            Right () -> Result "" Success

      putMVar resultVar result

    takeMVar resultVar


failureOnException :: IO Result -> IO Result
failureOnException action = catch action (\(e :: SomeException) -> return $ Result "Exception while evaluating example" (Failure Nothing (Error Nothing e)))

runWithBrowser :: Browser -> W.WD () -> WdExample
runWithBrowser browser action = WdExample browser action
