{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, FlexibleContexts, OverloadedStrings, NamedFieldPuns, LambdaCase #-}

module Test.Hspec.WebDriver.Internal.Hooks.Video (
  recordEntireVideo
  , recordIndividualVideos
  , recordErrorVideos
  ) where

import Control.Concurrent
import Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bits
import Data.Convertible
import Data.Maybe
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Test.Hspec.Core.Hooks
import Test.Hspec.Core.Spec
import Test.Hspec.WebDriver.Internal.Misc
import Test.Hspec.WebDriver.Internal.Types
import Test.Hspec.WebDriver.Internal.Util
import Test.WebDriver

#ifdef linux_HOST_OS
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
#endif

#ifdef darwin_HOST_OS
import Safe
#endif

-- * Hooks

-- | Record a single video of the entire test
recordEntireVideo :: (HasCallStack) => VideoSettings -> SpecType -> SpecType
recordEntireVideo videoSettings = beforeAllHook . afterAllHook
  where beforeAllHook = beforeAllWith $ \sess@(WdSession {wdOptions=(WdOptions {runRoot}), wdEntireTestRunVideo}) -> do
          modifyMVar_ wdEntireTestRunVideo $ \maybeProcess -> case maybeProcess of
            Nothing -> handle (\(e :: SomeException) -> putStrLn [i|Error in recordEntireVideo: '#{e}'|] >> return Nothing)
                              (Just <$> (startFullScreenVideoRecording (runRoot </> "video") videoSettings))
            Just _ -> return maybeProcess
          return sess

        afterAllHook = afterAll $ \(WdSession {wdEntireTestRunVideo}) -> do
          maybeVideoProcess <- readMVar wdEntireTestRunVideo
          whenJust maybeVideoProcess endVideoRecording

-- | Record videos of each test
recordIndividualVideos :: (HasCallStack) => VideoSettings -> SpecType -> SpecType
recordIndividualVideos videoSettings = aroundWith $ \action -> \session@(WdSession {wdLabels, wdOptions=(WdOptions {runRoot})}) -> do
  let resultsDir = (getResultsDir session)
  createDirectoryIfMissing True resultsDir
  E.bracket (startFullScreenVideoRecording (resultsDir </> "video") videoSettings)
            (endVideoRecording)
            (const $ action session)

-- | Record videos of each test, but delete them unless the test fails.
recordErrorVideos :: (HasCallStack) => VideoSettings -> SpecType -> SpecType
recordErrorVideos = undefined

-- * Video util functions

ffmpegOutfile :: String
ffmpegOutfile = "ffmpeg_stdout.txt"
ffmpegErrfile :: String
ffmpegErrfile = "ffmpeg_stderr.txt"

withVideoRecording :: FilePath -> VideoSettings -> WD () -> WD ()
withVideoRecording path videoSettings action = do
  (width, height) <- getWindowSize
  (x, y) <- getWindowPos
  withVideoRecording' path videoSettings (width, height, x, y) action

withFullScreenVideoRecording :: (MonadIO m, MonadBaseControl IO m) => FilePath -> VideoSettings -> m a -> m a
withFullScreenVideoRecording path videoSettings action = do
  (width, height) <- liftIO getScreenResolution
  withVideoRecording' path videoSettings (fromIntegral width, fromIntegral height, 0, 0) action

startWindowVideoRecording :: FilePath -> VideoSettings -> WD (Handle, Handle, ProcessHandle)
startWindowVideoRecording path videoSettings = do
  (width, height) <- getWindowSize
  (x, y) <- getWindowPos
  liftIO $ startVideoRecording path (width, height, x, y) videoSettings

startFullScreenVideoRecording :: (MonadIO m) => FilePath -> VideoSettings -> m (Handle, Handle, ProcessHandle)
startFullScreenVideoRecording path videoSettings  = do
  (width, height) <- liftIO getScreenResolution
  liftIO $ startVideoRecording path (fromIntegral width, fromIntegral height, 0, 0) videoSettings

startVideoRecording :: (MonadIO m) => FilePath -> (Word, Word, Int, Int) -> VideoSettings -> m (Handle, Handle, ProcessHandle)
startVideoRecording path (width, height, x, y) (VideoSettings {..}) = liftIO $ do
#ifdef linux_HOST_OS
  displayNum <- fromMaybe "" <$> (liftIO $ lookupEnv "DISPLAY")

  let executable = "ffmpeg"
  let cmd = ["-draw_mouse", (if hideMouseWhenRecording then "0" else "1")
            , "-y"
            , "-f", "x11grab"
            , "-r", "30"
            , "-s", [i|#{width}x#{height}|]
            , "-i", [i|#{displayNum}.0+#{x},#{y}|]]
            ++ x11grabOptions
            ++ [[i|#{path}.avi|]]
#endif

#ifdef darwin_HOST_OS
  maybeScreenNumber <- liftIO getMacScreenNumber
  let executable = "ffmpeg"
  let cmd = case maybeScreenNumber of
    Just screenNumber -> ["-y"
                         , "-f", "avfoundation"
                         , "-i", [i|#{screenNumber}|]]
                         ++ avfoundationOptions
                         ++ [[i|#{path}.avi|]]
    Nothing -> error [i|Not launching ffmpeg since OS X screen number couldn't be determined.|]
#endif

#ifdef mingw32_HOST_OS
  let executable = "ffmpeg.exe"
  let cmd = ["-f", "gdigrab"
            , "-draw_mouse", (if hideMouseWhenRecording then "0" else "1")
            , "-i", "desktop"]
            ++ gdigrabOptions
            ++ [[i|#{path}.mkv|]]
#endif

  liftIO $ removePathForcibly $ path <> "_" <> ffmpegErrfile
  liftIO $ removePathForcibly $ path <> "_" <> ffmpegOutfile
  outfile <- openFile (path <> "_" <> ffmpegOutfile) AppendMode
  errfile <- openFile (path <> "_" <>  ffmpegErrfile) AppendMode
  (_, _, _, h) <- createProcess $ (proc executable cmd)
                                  { std_in = Inherit
                                  , std_out = UseHandle outfile
                                  , std_err = UseHandle errfile }

  return (outfile, errfile, h)

endVideoRecording :: (MonadIO m) => (Handle, Handle, ProcessHandle) -> m ()
endVideoRecording (outfile, errfile, h) = liftIO $ do
  hClose outfile
  hClose errfile

  catch (interruptProcessGroupOf h)
        (\(e :: SomeException) -> putStrLn [i|Exception in interruptProcessGroupOf in endVideoRecording: #{e}|])

  waitForProcess h >>= \case
    ExitSuccess -> return ()
    ExitFailure n -> return ()
    -- ExitFailure n -> putStrLn [i|Error: ffmpeg exited with nonzero exit code #{n}'|]

withVideoRecording' :: (MonadIO m, MonadBaseControl IO m) => FilePath -> VideoSettings -> (Word, Word, Int, Int) -> m a -> m a
withVideoRecording' path videoSettings dimensions action =
  E.bracket (startVideoRecording path dimensions videoSettings)
            endVideoRecording
            (const action)

hideMouse :: IO ()
#ifdef linux_HOST_OS
-- | Some special code to hide the mouse cursor while recording videos
-- Taken from https://github.com/LeifW/xmonad-utils/blob/master/src/Utils.hs
-- Doesn't work without X11 so it's only enabled for Linux
hideMouse = do
  d <- openDisplay ""
  w  <- rootWindow d (defaultScreen d)

  let em = buttonPressMask .|. pointerMotionMask
  cursor <- nullCursor d w
  ps <- grabPointer d w False em grabModeAsync
                    grabModeAsync w cursor currentTime
  when (ps /= grabSuccess) $ do
    threadDelay 1000000
    hideMouse
  where
    nullCursor :: Display -> Window -> IO Cursor
    nullCursor d w = do
      let c = Color 0 0 0 0 0
      p <- createPixmap d w 1 1 1
      cursor <- createPixmapCursor d p p c c 0 0
      freePixmap d p
      return cursor

#else
hideMouse = return ()
#endif

-- * Screen resolution

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
