{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, LambdaCase, Rank2Types #-}

module Test.Hspec.WebDriver.Internal.WebDriver (
  startWebDriver
  , stopWebDriver
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Retry
import qualified Data.Aeson as A
import Data.Default
import Data.Either
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (PortNumber)
import Safe
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec.WebDriver.Internal.Binaries
import Test.Hspec.WebDriver.Internal.Hooks.Logs
import Test.Hspec.WebDriver.Internal.Ports
import Test.Hspec.WebDriver.Internal.Types
import qualified Test.WebDriver.Capabilities as W
import qualified Test.WebDriver.Config as W


#ifdef linux_HOST_OS
import System.Posix.IO
import System.Posix.Types
#endif

-- | Spin up a Selenium WebDriver and create a WdSession
startWebDriver :: WdOptions -> IO WdSession
startWebDriver wdOptions@(WdOptions {capabilities=capabilities', ..}) = do
  -- Set up config
  port <- findFreePortOrException
  let capabilities = case runMode of
        RunHeadless -> capabilities' { W.browser = browser'}
          where browser' = case W.browser capabilities of
                  x@(W.Chrome {..}) -> x { W.chromeOptions = "--headless":chromeOptions }
                  x -> error [i|Headless mode not yet supported for browser '#{x}'|]
        _ -> capabilities'
  let wdConfig = (def { W.wdPort = fromIntegral port, W.wdCapabilities = capabilities })

  -- Get the CreateProcess
  createDirectoryIfMissing True toolsRoot
  (wdCreateProcess, maybeDisplayFile) <- getWebdriverCreateProcess wdOptions port >>= \case
    Left err -> error [i|Failed to create webdriver process: '#{err}'|]
    Right x -> return x

  -- Open output handles
  let logsDir = runRoot </> "selenium_logs"
  createDirectoryIfMissing True logsDir
  hout <- openFile (logsDir </> seleniumOutFileName) AppendMode
  herr <- openFile (logsDir </> seleniumErrFileName) AppendMode

  -- Start the process and wait for it to be ready
  (_, _, _, p) <- createProcess $ wdCreateProcess {
    std_in = Inherit
    , std_out = UseHandle hout
    , std_err = UseHandle herr
    }
  -- Normally Selenium prints the ready message to stderr. However, when we're running under
  -- XVFB the two streams get combined and sent to stdout; see
  -- https://bugs.launchpad.net/ubuntu/+source/xorg-server/+bug/1059947
  -- As a result, we poll both files
  let readyMessage = "Selenium Server is up and running"
  -- Retry every 60ms, for up to 60s before admitting defeat
  let retryPolicy = constantDelay 60000 <> limitRetries 1000
  success <- retrying retryPolicy (\_retryStatus result -> return (not result)) $ const $
    T.readFile (logsDir </> seleniumErrFileName) >>= \case
      t | readyMessage `T.isInfixOf` t -> return True
      _ -> T.readFile (logsDir </> seleniumOutFileName) >>= \case
        t | readyMessage `T.isInfixOf` t -> return True
        _ -> return False
  unless success $ error [i|Selenium server failed to start after 60 seconds|]

  -- When a displayfd filepath is provided, try to obtain the X11 screen
  maybeXvfbSession <- case maybeDisplayFile of
    Nothing -> return Nothing
    Just (displayFilePath, (w, h)) -> do
      let retryPolicy = constantDelay 60000 <> limitRetries 1000
      recoverAll retryPolicy $ \_ ->
        readFile displayFilePath >>= \contents -> case readMay contents of
          Nothing -> throwIO $ userError [i|Couldn't determine X11 screen to use. Got data: '#{contents}'|]
          Just x -> return $ Just $ XvfbSession { xvfbDisplayNum = x
                                                , xvfbXauthority = runRoot </> ".Xauthority"
                                                , xvfbDimensions = (w, h) }

  -- Make the WdSession
  WdSession <$> pure []
            <*> pure (hout, herr, p, logsDir </> seleniumOutFileName, logsDir </> seleniumErrFileName, maybeXvfbSession)
            <*> pure wdOptions
            <*> newMVar []
            <*> newMVar 0
            <*> newMVar Nothing
            <*> newMVar (A.object [])
            <*> newMVar logFailureFn
            <*> pure wdConfig


stopWebDriver :: WdSession -> IO ()
stopWebDriver (WdSession {wdWebDriver=(hout, herr, h, _, _, _)}) = do
  terminateProcess h >> waitForProcess h
  hClose hout
  hClose herr

-- * Util

getWebdriverCreateProcess :: WdOptions -> PortNumber -> IO (Either T.Text (CreateProcess, Maybe (FilePath, (Int, Int))))
getWebdriverCreateProcess (WdOptions {toolsRoot, runMode, runRoot}) port = runExceptT $ do
  chromeDriverPath <- ExceptT $ downloadChromeDriverIfNecessary toolsRoot
  seleniumPath <- ExceptT $ downloadSeleniumIfNecessary toolsRoot

  case runMode of
    Normal -> return ((proc "java" [
                         [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                         , "-jar", seleniumPath
                         , "-port", show port
                         ]) { cwd = Just runRoot }
                         , Nothing)

    RunHeadless ->
      -- Headless mode is controlled in the capabilities
      return ((proc "java" [
                 [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                 , "-jar", seleniumPath
                 , "-port", show port
                 ]) { cwd = Just runRoot }
                 , Nothing)

#ifdef linux_HOST_OS
    RunInXvfb (XvfbConfig {xvfbResolution}) -> do
      let (w, h) = fromMaybe (1920, 1080) xvfbResolution
      liftIO $ createDirectoryIfMissing True runRoot

      tmpDir <- liftIO getCanonicalTemporaryDirectory
      (path, tmpHandle) <- liftIO $ openTempFile tmpDir "x11_server_num"
      Fd fd <- liftIO $ handleToFd tmpHandle

      -- For reasons beyond me at the moment, this works properly when run with shell but not with proc;
      -- the latter results in Chrome failing to start with Gtk-WARNING **: (date): cannot open display: :109
      -- TODO: allow verbose logging to be controlled with an option:
      return ((shell (unwords ["xvfb-run"
                             , "--auto-servernum"
                             , [i|--server-args="-screen 0 #{w}x#{h}x24 -displayfd #{fd}"|]
                             , [i|--auth-file="#{runRoot </> ".Xauthority"}"|]
                             , "java"
                             , [i|-Dwebdriver.chrome.driver=#{chromeDriverPath}|]
                             , [i|-Dwebdriver.chrome.logfile=#{runRoot </> "chromedriver.log"}|]
                             , [i|-Dwebdriver.chrome.verboseLogging=true|]
                             , "-jar", seleniumPath
                             , "-port", show port
                             ])) { cwd = Just runRoot }, Just (path, (w, h)))

#else
    RunInXvfb (XvfbConfig { xvfbResolution }) -> error [i|RunInXvfb can only be used on Linux.|]
#endif
