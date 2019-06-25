{-# LANGUAGE CPP, ConstraintKinds, QuasiQuotes, TemplateHaskell, LambdaCase #-}

module Test.Hspec.WebDriver.Toolkit.Waits where

import Control.Concurrent
import qualified Control.Exception as E
import qualified Control.Exception.Lifted as EL
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.Convertible
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Stack
import Network.HTTP
import Network.Stream hiding (Result)
import System.Timeout
import Test.Hspec.WebDriver.Toolkit.Expectations
import Test.WebDriver
import Test.WebDriver.Session (WDSessionStateIO, WDSessionStateControl, WDSessionState)


implicitWait :: Integer
implicitWait = 20000
scriptTimeout :: Integer
scriptTimeout = 20000
pageLoadTimeout :: Integer
pageLoadTimeout = 300000 -- 5 minutes in ms

waitUntilURLContains :: (HasCallStack) => T.Text -> WD ()
waitUntilURLContains desired = waitUntil 10 $ getCurrentURL >>= ((`textShouldContain` desired) . convert)

waitForElementToDisappear :: (HasCallStack) => T.Text -> WD ()
waitForElementToDisappear modalSelector = waitUntil 10 $ elementShouldNotExist modalSelector

elementShouldExist :: (HasCallStack) => T.Text -> WD ()
elementShouldExist css = numElements css >>= \case
  0 -> expectationFailure [i|Element '#{css}' didn't exist|]
  _ -> return ()

elementShouldNotExist :: (HasCallStack) => T.Text -> WD ()
elementShouldNotExist css = numElements css >>= \case
  0 -> return ()
  n -> expectationFailure [i|Element '#{css}' still existed (#{n})|]

numElements :: (HasCallStack) => T.Text -> WD Int
numElements css = withImplicitWaitMs 100 $ length <$> findElems (ByCSS css)

numberElementsShouldBe :: (HasCallStack) => T.Text -> Int -> WD ()
numberElementsShouldBe css n = numElements css >>= (\result -> (css, result) `shouldBe` (css, n))

withImplicitWaitMs :: (HasCallStack) => Integer -> WD a -> WD a
withImplicitWaitMs wait action = EL.bracket (setImplicitWait wait)
                                            (const $ setImplicitWait implicitWait)
                                            (const action)

-- | Send HTTP requests to url until we get a 200 response
waitUntil200 :: (HasCallStack) => String -> IO ()
waitUntil200 url = do
  response <- E.handle (\(_ :: E.SomeException) -> return $ Left $ ErrorMisc "No good") $ simpleHTTP (getRequest url)
  case response of
    Right (Response {rspCode}) | rspCode == (2, 0, 0) -> return ()
    _ -> retry
  where retry = threadDelay 1000000 >> waitUntil200 url

-- | Same as waitUntil200, but with a fixed timeout
waitUntil200WithTimeout :: (HasCallStack) => String -> IO ()
waitUntil200WithTimeout = waitUntil200WithTimeout' 30000000

-- | Same as waitUntil200WithTimeout, but with a customizable timeout
waitUntil200WithTimeout' :: (HasCallStack) => Int -> String -> IO ()
waitUntil200WithTimeout' timeInMicroseconds url = do
  maybeSuccess <- timeout timeInMicroseconds $ waitUntil200 url
  when (isNothing maybeSuccess) $ error [i|Failed to connect to URL "#{url}" in waitUntil200WithTimeout'...|]


-- * Below is a modified version of Test.WebDriver.Commands.waitUntil which is less silly.
-- The original code wants to catch an ExpectFailure, which is an exception type it made up.
-- The ExpectFailure contains a string, and so the process of wrapping up an actual exception
-- such as an HUnitFailure makes it harder to deal with.
-- This is especially bad since test runners like Hspec and Tasty often know how to interpret
-- common exceptions like HUnitFailure and pretty print them nicely in test outputs.
---------------------------------------------------------

waitUntil :: (HasCallStack, MonadCatch m, MonadIO m, MonadBaseControl IO m, WDSessionState m) => Double -> m a -> m a
waitUntil = waitUntil' 500000

-- |Similar to 'waitUntil' but allows you to also specify the poll frequency
-- of the 'WD' action. The frequency is expressed as an integer in microseconds.
waitUntil' :: (HasCallStack, WDSessionStateControl m, MonadIO m) => Int -> Double -> m a -> m a
waitUntil' = waitEither id (const return)

-- |Internal function used to implement explicit wait commands using success and failure continuations
waitEither :: (HasCallStack, WDSessionStateControl m, MonadIO m) =>
              ((EL.SomeException -> m b) -> EL.SomeException -> m b)
           -> ((EL.SomeException -> m b) -> a -> m b)
           -> Int -> Double -> m a -> m b
waitEither failure success = wait' handler
 where
  handler retry wd = do
    e <- fmap Right wd  `EL.catches` [EL.Handler handleFailedCommand
                                     , EL.Handler handleOtherException
                                     ]
    either (failure retry) (success retry) e
   where
    handleFailedCommand e@(FailedCommand NoSuchElement _) = return $ Left $ EL.SomeException e
    handleFailedCommand err = EL.throwIO err

    handleOtherException (e :: EL.SomeException) = return $ Left e

wait' :: (HasCallStack, WDSessionStateIO m) =>
         ((EL.SomeException -> m b) -> m a -> m b) -> Int -> Double -> m a -> m b
wait' handler waitAmnt t wd = waitLoop =<< liftBase getCurrentTime
  where
    timeout = realToFrac t
    waitLoop startTime = handler retry wd
      where
        retry why = do
          now <- liftBase getCurrentTime
          if diffUTCTime now startTime >= timeout
            then EL.throwIO why
            else do
              liftBase . threadDelay $ waitAmnt
              waitLoop startTime
