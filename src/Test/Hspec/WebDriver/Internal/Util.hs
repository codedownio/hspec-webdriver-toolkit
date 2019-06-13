{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns #-}

module Test.Hspec.WebDriver.Internal.Util where

import Control.Concurrent
import Control.Exception
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Convertible
import Data.Default
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Network.Socket (PortNumber)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec
import qualified Test.Hspec as H
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Internal.Types
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W

getResultsDir' :: FilePath -> [String] -> FilePath
getResultsDir' runRoot labels = runRoot </> "results" </> (L.intercalate "/" (reverse labels))

getResultsDir :: WdSession -> FilePath
getResultsDir (WdSession {wdOptions=(WdOptions {runRoot}), wdLabels}) = getResultsDir' runRoot wdLabels

-- * Truncating log files

moveAndTruncate :: FilePath -> String -> IO ()
moveAndTruncate from to = do
  exists <- doesFileExist from
  when exists $ do
    copyFile from to
    tryTruncateFile from

tryTruncateFile :: FilePath -> IO ()
tryTruncateFile path = E.catch (truncateFile path)
                               (\(e :: E.SomeException) -> putStrLn [i|Failed to truncate file #{path}: #{e}|])

truncateFile :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
truncateFile path = withFile path WriteMode $ flip hPutStr "\n" -- Not exactly truncation, but close enough?
#else
truncateFile path = void $ readCreateProcess (shell [i|> #{path}|]) ""
#endif

-- * Exceptions

leftOnException :: (MonadIO m, MonadBaseControl IO m) => m (Either T.Text a) -> m (Either T.Text a)
leftOnException = E.handle (\(e :: SomeException) -> return $ Left $ convert $ show e)

leftOnException' :: (MonadIO m, MonadBaseControl IO m) => m a -> m (Either T.Text a)
leftOnException' action = E.catch (Right <$> action) (\(e :: SomeException) -> return $ Left $ convert $ show e)

-- * Util

whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) action = void $ action x

whenLeft :: (Monad m) => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) action = action x
whenLeft (Right _) _ = return ()

whenRight :: (Monad m) => Either a b -> (b -> m ()) -> m ()
whenRight (Left _) _ = return ()
whenRight (Right x) action = action x
