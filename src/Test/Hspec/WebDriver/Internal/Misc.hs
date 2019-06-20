{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, Rank2Types #-}

module Test.Hspec.WebDriver.Internal.Misc (
  beforeWith'
  , beforeAllWith
  ) where


import Control.Concurrent
import qualified Control.Exception as E
import qualified Test.Hspec as H
import Test.Hspec.Core.Spec


-- * Note: sent a PR to add this hook to hspec-core:
-- https://github.com/hspec/hspec/pull/417
-- Need to update it with latest version

-- | Run a custom action before every spec item, with access to the SpecWith contents and without altering the contents.
beforeWith' :: (a -> IO ()) -> SpecWith a -> SpecWith a
beforeWith' action = H.aroundWith $ \actionExpectingA aValue -> do
  action aValue
  actionExpectingA aValue

-- | Run a custom action before the first spec item, with access to the SpecWith contents and without altering the contents.
beforeAllWith :: (a -> IO ()) -> SpecWith a -> SpecWith a
beforeAllWith action spec = do
  mvar <- runIO (newMVar Empty)

  flip H.aroundWith spec $ \actionExpectingA aValue -> do
    memoize' mvar action aValue
    actionExpectingA aValue

data Memoized = Empty
              | Memoized
              | Failed E.SomeException

memoize' :: MVar Memoized -> (a -> IO ()) -> (a -> IO ())
memoize' mvar action x = do
  result <- modifyMVar mvar $ \ma -> case ma of
    Empty -> do
      result <- E.try $ action x
      case result of
        Left err -> return (Failed err, Left err)
        Right () -> return (Memoized, Right ())
    Memoized -> return (ma, Right ())
    Failed _ -> return (ma, Right ())

  case result of
    Left err -> E.throwIO err
    Right () -> return ()
