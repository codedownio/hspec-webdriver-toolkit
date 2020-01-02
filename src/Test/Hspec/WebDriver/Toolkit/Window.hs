{-# LANGUAGE CPP, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Hspec.WebDriver.Toolkit.Window where

import Control.Concurrent
import Control.Exception.Lifted as EL
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits as B
import qualified Data.ByteString as B
import Data.Convertible
import Data.List
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import System.Environment
import System.Process
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Toolkit.Expectations
import Test.Hspec.WebDriver.Toolkit.Waits
import Test.WebDriver
import qualified Test.WebDriver as W
import Test.WebDriver.Exceptions

setWindowLeftSide :: (HasCallStack) => WdSession -> WD ()
setWindowLeftSide sess = do
  (width, height) <- liftIO $ getScreenResolution sess
  setWindowPos (0, 0)
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowRightSide :: (HasCallStack) => WdSession -> WD ()
setWindowRightSide sess = do
  (width, height) <- liftIO $ getScreenResolution sess
  let pos = (fromIntegral $ B.shift width (-1), 0)
  setWindowPos pos
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowFullScreen :: (HasCallStack) => WdSession -> WD ()
setWindowFullScreen sess = do
  (width, height) <- liftIO $ getScreenResolution sess
  setWindowPos (0, 0)
  setWindowSize (fromIntegral width, fromIntegral height)

whenHeadlessDisplay :: (HasCallStack) => WD () -> WD ()
whenHeadlessDisplay action = do
  (liftIO $ lookupEnv "DISPLAY") >>= \case
    Just x | (x /= ":0") && (x /= ":1") -> action
    _ -> return ()

withWindowFullscreen :: (HasCallStack) => WdSession -> IO a -> WD a
withWindowFullscreen sess action = do
  originalPos <- getWindowPos
  originalSize <- getWindowSize

  bracket (setWindowFullScreen sess)
          (\() -> setWindowPos originalPos >> setWindowSize originalSize)
          (\() -> liftIO action)

getScreenResolution :: (HasCallStack) => WdSession -> IO (Int, Int)
getScreenResolution (WdSession {wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) = do
  envArg <- case maybeXvfbSession of
    Nothing -> return Nothing
    Just (XvfbSession {..}) -> do
      -- Use same environment as shell, but replace DISPLAY arg
      env' <- liftIO getEnvironment
      return $ Just $ L.nubBy (\x y -> fst x == fst y) $ [("DISPLAY", ":" <> show xvfbDisplayNum)
                                                         , ("XAUTHORITY", xvfbXauthority)] <> env'

  result <- readCreateProcess ((shell [i|xdpyinfo | grep dimensions|]) { env=envArg }) ""
  let resolutionPart = (T.words $ convert result) !! 1
  let (widthText:heightText:_) = T.splitOn "x" resolutionPart
  return (read $ convert $ widthText, read $ convert heightText)

-- | Ensure that any extra windows opened over the course of an action are closed after it
closingExtraWindows :: (HasCallStack) => WD () -> WD ()
closingExtraWindows action =
  bracket (do
              originalFocusedWindow <- getCurrentWindow
              originalWindows <- windows
              return (originalFocusedWindow, originalWindows)
          )
          (\(originalFocusedWindow, originalWindows) -> do
              curWindows <- windows
              forM_ (curWindows \\ originalWindows) $ \toClose ->
                handle swallowNoSuchWindowException (closeWindow toClose)

              handle swallowNoSuchWindowException (focusWindow originalFocusedWindow)
          )
          (const action)

-- | Assert that a window should pop up while performing an action.
-- Then, close the newly opened window before returning.
-- Used to test the file downloads open.
windowShouldOpen :: (HasCallStack) => WD () -> WD ()
windowShouldOpen action = do
  originalWindows <- windows

  action

  -- If this export succeeds, a new window opens with the PDF export.
  -- Wait for the window to appear.
  curWindows <- waitUntil 10 $ do
    curWindows <- windows
    (length (curWindows \\ originalWindows)) `shouldBe` 1
    return curWindows

  -- Close the newly opened window
  let windowToClose = head (curWindows \\ originalWindows)
  focusWindow windowToClose
  closeWindow windowToClose

  focusWindow $ head originalWindows

swallowNoSuchWindowException (FailedCommand NoSuchWindow _) = return ()
swallowNoSuchWindowException e = EL.throw e

closeAllWindowsExceptCurrentInAllSessions (WdSession {wdSessionMap}) = do
  sessionMap <- readMVar wdSessionMap
  forM_ (M.toList sessionMap) $ \(_, sess) -> W.runWD sess closeAllWindowsExceptCurrent

closeAllWindowsExceptCurrent = do
  cur <- getCurrentWindow
  hs <- windows
  forM_ [x | x <- hs, x /= cur] $ \h -> catch (closeWindow h) $ \(e :: FailedCommand) ->
    liftIO $ putStrLn [i|Failed to close window: '#{e}'|]
