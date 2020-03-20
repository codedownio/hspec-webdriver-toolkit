{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables #-}

module Test.Hspec.WebDriver.Internal.Ports (
  findFreePortOrException
  ) where

import Control.Exception
import Control.Retry
import Data.Maybe
import qualified Network.Socket as N
import System.Random (randomRIO)

-- |Find an unused port in a given range
findFreePortInRange' :: RetryPolicy -> (N.PortNumber, N.PortNumber) -> [N.PortNumber] -> IO (Maybe N.PortNumber)
findFreePortInRange' retryPolicy (start, end) blacklist = retrying retryPolicy (\_retryStatus result -> return $ isNothing result) (const findFreePortInRange')
  where getAcceptableCandidate :: IO N.PortNumber
        getAcceptableCandidate = do
          candidate <- (fromInteger) <$> randomRIO (fromIntegral start, fromIntegral end)
          if | candidate `elem` blacklist -> getAcceptableCandidate
             | otherwise -> return candidate

        findFreePortInRange' :: IO (Maybe N.PortNumber)
        findFreePortInRange' = do
          candidate <- getAcceptableCandidate
          catch (tryOpenAndClosePort candidate >> return (Just candidate)) (\(_ :: SomeException) -> return Nothing)
          where
            tryOpenAndClosePort :: N.PortNumber -> IO N.PortNumber
            tryOpenAndClosePort port = do
              sock <- N.socket N.AF_INET N.Stream 0
              N.setSocketOption sock N.ReuseAddr 1
              hostAddress <- N.inet_addr "127.0.0.1"
              N.bind sock (N.SockAddrInet port hostAddress)
              N.close sock
              return $ fromIntegral port


findFreePortInRange :: (N.PortNumber, N.PortNumber) -> [N.PortNumber] -> IO (Maybe N.PortNumber)
findFreePortInRange = findFreePortInRange' (limitRetries 50)

-- |Find an unused port in the ephemeral port range.
-- See https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
-- This works without a timeout since there should always be a port in the somewhere;
-- it might be advisable to wrap in a timeout anyway.
findFreePort :: IO (Maybe N.PortNumber)
findFreePort = findFreePortInRange (49152, 65535) []

findFreePortOrException :: IO N.PortNumber
findFreePortOrException = findFreePort >>= \case
  Just port -> return port
  Nothing -> error "Couldn't find free port"

-- findFreePortNotIn :: [N.PortNumber] -> IO (Maybe N.PortNumber)
-- findFreePortNotIn = findFreePortInRange (49152, 65535)
