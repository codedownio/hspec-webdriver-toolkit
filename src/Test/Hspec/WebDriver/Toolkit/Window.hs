{-# LANGUAGE CPP, QuasiQuotes, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Hspec.WebDriver.Toolkit.Window where

import Control.Exception.Lifted as EL
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits as B
import qualified Data.ByteString as B
import Data.Convertible
import Data.List
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import System.Environment
import System.Process
import Test.Hspec.WebDriver.Toolkit.Expectations
import Test.Hspec.WebDriver.Toolkit.Waits
import Test.WebDriver
import Test.WebDriver.Exceptions

setWindowLeftSide :: (HasCallStack) => WD ()
setWindowLeftSide = do
  (width, height) <- liftIO getScreenResolution
  setWindowPos (0, 0)
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowRightSide :: (HasCallStack) => WD ()
setWindowRightSide = do
  (width, height) <- liftIO getScreenResolution
  let pos = (fromIntegral $ B.shift width (-1), 0)
  setWindowPos pos
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowFullScreen :: (HasCallStack) => WD ()
setWindowFullScreen = do
  (width, height) <- liftIO getScreenResolution
  setWindowPos (0, 0)
  setWindowSize (fromIntegral width, fromIntegral height)

whenHeadlessDisplay :: (HasCallStack) => WD () -> WD ()
whenHeadlessDisplay action = do
  (liftIO $ lookupEnv "DISPLAY") >>= \case
    Just x | (x /= ":0") && (x /= ":1") -> action
    _ -> return ()

withWindowFullscreen :: (HasCallStack) => IO a -> WD a
withWindowFullscreen action = do
  originalPos <- getWindowPos
  originalSize <- getWindowSize

  bracket (setWindowFullScreen)
          (\() -> setWindowPos originalPos >> setWindowSize originalSize)
          (\() -> liftIO action)

#ifdef linux_HOST_OS
getScreenResolution :: (HasCallStack) => IO (Int, Int)
getScreenResolution = do
  result <- readCreateProcess (shell [i|xdpyinfo | grep dimensions|]) ""
  let resolutionPart = (T.words $ convert result) !! 1
  let (widthText:heightText:_) = T.splitOn "x" resolutionPart
  return (read $ convert $ widthText, read $ convert heightText)
#endif

#ifdef darwin_HOST_OS
getScreenResolution :: (HasCallStack) => IO (Int, Int)
getScreenResolution = do
  -- TODO: get this for real somehow
  return (1600, 900)

getMacScreenNumber :: (HasCallStack) => IO (Maybe Int)
getMacScreenNumber =
  readMay <$> readCreateProcess (shell [i|ffmpeg -f avfoundation -list_devices true -i "" 2>&1 | grep "Capture screen 0" | grep -Eo "\\[(\\d+)\\]" | sed 's/\\[//' | sed 's/\\]//'|]) ""
#endif

#ifdef mingw32_HOST_OS
-- TODO: define getScreenResolution for WindowsgetScreenResolution :: IO (Int, Int)
getScreenResolution :: (HasCallStack) => IO (Int, Int)
getScreenResolution = do
  -- TODO: get this for real somehow
  return (1600, 900)
#endif

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
                handle swallowNoSuchWindowException $ focusWindow toClose >> closeWindow toClose

              handle swallowNoSuchWindowException $ focusWindow originalFocusedWindow
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
