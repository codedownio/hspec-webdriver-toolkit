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
  where beforeAllHook = beforeAllWith $ \sess@(WdSession {wdOptions=(WdOptions {runRoot}), wdEntireTestRunVideo, wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) -> do
          modifyMVar_ wdEntireTestRunVideo $ \maybeProcess -> case maybeProcess of
            Nothing -> handle (\(e :: SomeException) -> putStrLn [i|Error in recordEntireVideo: '#{e}'|] >> return Nothing)
                              (Just <$> (startFullScreenVideoRecording (runRoot </> "video") videoSettings maybeXvfbSession))
            Just _ -> return maybeProcess

        afterAllHook = afterAll $ \(WdSession {wdEntireTestRunVideo}) -> do
          maybeVideoProcess <- readMVar wdEntireTestRunVideo
          whenJust maybeVideoProcess $ \(hout, herr, h, _) -> endVideoRecording (hout, herr, h)

-- | Record videos of each test
recordIndividualVideos :: (HasCallStack) => VideoSettings -> SpecType -> SpecType
recordIndividualVideos videoSettings = aroundWith $ \action session@(WdSession {wdLabels, wdOptions=(WdOptions {runRoot}), wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) -> do
  let resultsDir = (getResultsDir session)
  createDirectoryIfMissing True resultsDir
  E.bracket (startFullScreenVideoRecording (resultsDir </> "individual_video") videoSettings maybeXvfbSession)
            (\(hout, herr, h, _) -> endVideoRecording (hout, herr, h))
            (const $ action session)

-- | Record videos of each test, but delete them unless the test fails.
recordErrorVideos :: (HasCallStack) => VideoSettings -> SpecType -> SpecType
recordErrorVideos videoSettings = aroundWithHook
  where
    aroundWithHook = aroundWith $ \action session@(WdSession {wdLabels, wdOptions=(WdOptions {runRoot}), wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) -> do
      let resultsDir = getResultsDir session
      createDirectoryIfMissing True resultsDir
      testFailedVar <- newMVar False
      E.bracket (startFullScreenVideoRecording (resultsDir </> "error_video") videoSettings maybeXvfbSession)
                (\(hout, herr, h, path) -> do
                    endVideoRecording (hout, herr, h)
                    testFailed <- readMVar testFailedVar
                    unless testFailed $ do
                      removePathForcibly path
                      removePathForcibly (resultsDir </> ("error_video_" <> ffmpegOutfile))
                      removePathForcibly (resultsDir </> ("error_video_" <> ffmpegErrfile))
                )
                (const $ (try $ action session) >>= \case
                    Left (err :: SomeException) -> do
                      modifyMVar_ testFailedVar $ const $ return True
                      throwIO err
                    Right () -> return ()
                )

-- * Video util functions

ffmpegOutfile :: String
ffmpegOutfile = "ffmpeg_stdout.txt"
ffmpegErrfile :: String
ffmpegErrfile = "ffmpeg_stderr.txt"

withVideoRecording :: FilePath -> VideoSettings -> Maybe XvfbSession -> WD () -> WD ()
withVideoRecording path videoSettings maybeXvfbSession action = do
  (width, height) <- getWindowSize
  (x, y) <- getWindowPos
  withVideoRecording' path videoSettings maybeXvfbSession (width, height, x, y) action

withFullScreenVideoRecording :: (MonadIO m, MonadBaseControl IO m) => FilePath -> VideoSettings -> Maybe XvfbSession -> m a -> m a
withFullScreenVideoRecording path videoSettings maybeXvfbSession action = do
  (width, height) <- liftIO getScreenResolution
  withVideoRecording' path videoSettings maybeXvfbSession (fromIntegral width, fromIntegral height, 0, 0) action

startWindowVideoRecording :: FilePath -> VideoSettings -> Maybe XvfbSession -> WD (Handle, Handle, ProcessHandle, FilePath)
startWindowVideoRecording path videoSettings maybeXvfbSession = do
  (width, height) <- getWindowSize
  (x, y) <- getWindowPos
  liftIO $ startVideoRecording path (width, height, x, y) videoSettings maybeXvfbSession

startFullScreenVideoRecording :: (MonadIO m) => FilePath -> VideoSettings -> Maybe XvfbSession -> m (Handle, Handle, ProcessHandle, FilePath)
startFullScreenVideoRecording path videoSettings maybeXvfbSession = do
  (width, height) <- case maybeXvfbSession of
    Just (XvfbSession {xvfbDimensions}) -> return xvfbDimensions
    Nothing -> liftIO getScreenResolution
  liftIO $ startVideoRecording path (fromIntegral width, fromIntegral height, 0, 0) videoSettings maybeXvfbSession

startVideoRecording :: (MonadIO m) => FilePath -> (Word, Word, Int, Int) -> VideoSettings -> Maybe XvfbSession -> m (Handle, Handle, ProcessHandle, FilePath)
startVideoRecording path (width, height, x, y) (VideoSettings {..}) maybeXvfbSession = liftIO $ do
#ifdef linux_HOST_OS
  displayNum <- case maybeXvfbSession of
    Nothing -> fromMaybe "" <$> (liftIO $ lookupEnv "DISPLAY")
    Just (XvfbSession {xvfbDisplayNum}) -> return $ ":" <> show xvfbDisplayNum



  let executable = "ffmpeg"
  let videoPath = [i|#{path}.avi|]
  let env' = [("DISPLAY", displayNum)]
  let env = case maybeXvfbSession of
       Nothing -> Just env'
       Just (XvfbSession {xvfbXauthority}) -> Just (("XAUTHORITY", xvfbXauthority) : env')
  let cmd = ["-draw_mouse", (if hideMouseWhenRecording then "0" else "1")
            , "-y"
            , "-nostdin"
            , "-f", "x11grab"
            , "-s", [i|#{width}x#{height}|]
            , "-i", [i|#{displayNum}.0+#{x},#{y}|]]
            ++ x11grabOptions
            ++ [videoPath]
#endif

#ifdef darwin_HOST_OS
  maybeScreenNumber <- liftIO getMacScreenNumber
  let executable = "ffmpeg"
  let videoPath = [i|#{path}.avi|]
  let env = Nothing
  let cmd = case maybeScreenNumber of
    Just screenNumber -> ["-y"
                         , "-nostdin"
                         , "-f", "avfoundation"
                         , "-i", [i|#{screenNumber}|]]
                         ++ avfoundationOptions
                         ++ [videoPath]
    Nothing -> error [i|Not launching ffmpeg since OS X screen number couldn't be determined.|]
#endif

#ifdef mingw32_HOST_OS
  let executable = "ffmpeg.exe"
  let videoPath = [i|#{path}.mkv|]
  let env = Nothing
  let cmd = ["-f", "gdigrab"
            , "-nostdin"
            , "-draw_mouse", (if hideMouseWhenRecording then "0" else "1")
            , "-i", "desktop"]
            ++ gdigrabOptions
            ++ [videoPath]
#endif

  liftIO $ removePathForcibly $ path <> "_" <> ffmpegErrfile
  liftIO $ removePathForcibly $ path <> "_" <> ffmpegOutfile
  outfile <- openFile (path <> "_" <> ffmpegOutfile) AppendMode
  errfile <- openFile (path <> "_" <>  ffmpegErrfile) AppendMode
  (_, _, _, h) <- createProcess $ (proc executable cmd)
                                  { create_group = True
                                  , std_in = NoStream
                                  , std_out = UseHandle outfile
                                  , std_err = UseHandle errfile
                                  , env = env }

  return (outfile, errfile, h, videoPath)

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

withVideoRecording' :: (MonadIO m, MonadBaseControl IO m) => FilePath -> VideoSettings -> Maybe XvfbSession -> (Word, Word, Int, Int) -> m a -> m a
withVideoRecording' path videoSettings maybeXvfbSession dimensions action =
  E.bracket (startVideoRecording path dimensions videoSettings maybeXvfbSession)
            (\(hout, herr, h, _) -> endVideoRecording (hout, herr, h))
            (const action)

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
