{-# LANGUAGE NamedFieldPuns, RecordWildCards, QuasiQuotes, ScopedTypeVariables #-}

import Binaries
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.String.Interpolate.IsString
import Lib
import Test.Hspec
import Test.Hspec.Core.Spec
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W
import Wrap

type SpecType = SpecWith WdSessionWithLabels

beforeAction :: WdSessionWithLabels -> IO WdSessionWithLabels
beforeAction sess@(WdSessionWithLabels {wdLabels}) = do
  putStrLn $ "beforeAction called with labels: " ++ show wdLabels
  return sess

afterAction (WdSessionWithLabels {wdLabels}) = do
  putStrLn $ "afterAction called with labels: " ++ show wdLabels

tests :: SpecType
tests = describe "Basic widget tests" $ beforeWith beforeAction $ after afterAction $ do
  describe "Basic editing" $ do
    it "does the first thing" $ \(WdSessionWithLabels {..}) -> do
      putStrLn $ "Doing the first thing: " <> show wdLabels

    it "does the second thing" $ \(WdSessionWithLabels {..}) -> do
      putStrLn $ "Doing the first thing: " <> show wdLabels

    it "starts a browser" $ runWithBrowser "browser1" $ do
      openPage "http://www.google.com"

    it "starts another browser" $ runWithBrowser "browser2" $ do
      openPage "http://www.yahoo.com"

main :: IO ()
main = do
  sessionMap <- newMVar []

  let wdOptions = def
  let caps = W.defaultCaps { W.browser = W.chrome }

  withWebDriver "/tmp/testroot" Nothing $ \baseConfig -> do
    let wdConfig = baseConfig { W.wdCapabilities = caps }
    let sess = WdSession wdOptions sessionMap wdConfig
    let initialSessionWithLabels = WdSessionWithLabels [] sess

    hspec $ beforeAll (return initialSessionWithLabels) $
      afterAll closeAllSessions $
      addLabelsToTree (\labels sessionWithLabels -> sessionWithLabels { wdLabels = labels }) $
      tests


closeAllSessions :: WdSessionWithLabels -> IO ()
closeAllSessions (WdSessionWithLabels {wdSession=(WdSession {wdSessionMap})}) = do
  sessionMap <- readMVar wdSessionMap
  forM_ sessionMap $ \(name, sess) -> do
    putStrLn [i|Closing session '#{name}'|]
    catch (W.runWD sess closeSession)
          (\(e :: SomeException) -> putStrLn [i|Failed to destroy session '#{name}': #{e}|])
