{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, LambdaCase, Rank2Types #-}

module Test.Hspec.WebDriver.Internal.Misc (
  beforeAllWith
  ) where


import Control.Concurrent
import qualified Control.Exception as E
import qualified Test.Hspec as H
import Test.Hspec.Core.Spec


-- * Note: sent a PR to add this hook to hspec-core:
-- https://github.com/hspec/hspec/pull/417

-- | Run a custom action before the first spec item, with access to the SpecWith contents.
-- This is missing from hspec-core; see https://github.com/hspec/hspec/pull/417
beforeAllWith :: (b -> IO a) -> SpecWith a -> SpecWith b
beforeAllWith action spec = do
  mvar <- runIO (newMVar Empty)
  H.beforeWith (memoize' mvar action) spec

data Memoized a =
    Empty
  | Memoized a
  | Failed E.SomeException

memoize' :: MVar (Memoized a) -> (b -> IO a) -> (b -> IO a)
memoize' mvar action x = do
  result <- modifyMVar mvar $ \ma -> case ma of
    Empty -> do
      a <- E.try $ action x
      return (either Failed Memoized a, a)
    Memoized a -> return (ma, Right a)
    Failed _ -> E.throwIO (Pending Nothing (Just "exception in beforeAll-hook (see previous failure)"))
  either E.throwIO return result
