{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

import Binaries
import Control.Concurrent
import Data.Default
import Lib
import Test.Hspec
import Test.Hspec.Core.Spec
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W
import Wrap

type SpecType = SpecWith WdSessionWithLabels

beforeAction :: WdSessionWithLabels -> IO WdSessionWithLabels
beforeAction sess@(WdSessionWithLabels {wdLabels}) = do
  putStrLn $ "beforeAction called with labels: " ++ show wdLabels
  return sess

afterAction (WdSessionWithLabels {wdLabels}) = putStrLn $ "afterAction called with labels: " ++ show wdLabels

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

  withWebDriver "/tmp/testroot" $ \baseConfig -> do
    let wdConfig = baseConfig { W.wdCapabilities = caps }
    let sess = WdSession wdOptions sessionMap wdConfig
    let initialSessionWithLabels = WdSessionWithLabels [] sess

    hspec $ beforeAll (return initialSessionWithLabels) $ addLabelsToTree (\labels sessionWithLabels -> sessionWithLabels { wdLabels = labels }) $ tests
