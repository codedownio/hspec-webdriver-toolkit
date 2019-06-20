{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns, MultiWayIf, ViewPatterns #-}

module Test.Hspec.WebDriver.Internal.Binaries.Util (
  detectPlatform
  , detectChromeMajorVersion
  , Platform(..)
  , chromeDriverPaths
  ) where

import Data.Convertible
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified System.Info as SI
import System.Process
import Test.Hspec.WebDriver.Internal.Util

data Platform = Linux | OSX | Windows deriving (Show, Eq)

detectPlatform :: Platform
detectPlatform =  case SI.os of
  "windows" -> Windows
  "linux" -> Linux
  "darwin" -> OSX
  _ -> error [i|Couldn't determine host platform from string: '#{SI.os}'|]


chromeDriverPaths :: [(Int, [(Platform, T.Text)])]
chromeDriverPaths = [
  (76, [(Linux, "https://chromedriver.storage.googleapis.com/76.0.3809.12/chromedriver_linux64.zip")
       , (OSX, "https://chromedriver.storage.googleapis.com/76.0.3809.12/chromedriver_mac64.zip")
       , (Windows, "https://chromedriver.storage.googleapis.com/76.0.3809.12/chromedriver_win32.zip")])

  , (75, [(Linux, "https://chromedriver.storage.googleapis.com/75.0.3770.8/chromedriver_linux64.zip")
         , (OSX, "https://chromedriver.storage.googleapis.com/75.0.3770.8/chromedriver_mac64.zip")
         , (Windows, "https://chromedriver.storage.googleapis.com/75.0.3770.8/chromedriver_win32.zip")])

  , (74, [(Linux, "https://chromedriver.storage.googleapis.com/74.0.3729.6/chromedriver_linux64.zip")
         , (OSX, "https://chromedriver.storage.googleapis.com/74.0.3729.6/chromedriver_mac64.zip")
         , (Windows, "https://chromedriver.storage.googleapis.com/74.0.3729.6/chromedriver_win32.zip")])

  , (73, [(Linux, "https://chromedriver.storage.googleapis.com/73.0.3683.68/chromedriver_linux64.zip")
         , (OSX, "https://chromedriver.storage.googleapis.com/73.0.3683.68/chromedriver_mac64.zip")
         , (Windows, "https://chromedriver.storage.googleapis.com/73.0.3683.68/chromedriver_win32.zip")])

  , (72, chromeDriver2_46)
  , (71, chromeDriver2_46)
  , (70, chromeDriver2_46)
  ]

chromeDriver2_46 = [(Linux, "https://chromedriver.storage.googleapis.com/2.46/chromedriver_linux64.zip")
                   , (OSX, "https://chromedriver.storage.googleapis.com/2.46/chromedriver_mac64.zip")
                   , (Windows, "https://chromedriver.storage.googleapis.com/2.46/chromedriver_win32.zip")]

-- | TODO: make sure this works on other platforms
-- TODO: actually parse this out
detectChromeMajorVersion :: IO (Either T.Text Int)
detectChromeMajorVersion = leftOnException $ do
  (convert -> stdout) <- readCreateProcess (proc "google-chrome" ["--version"]) ""
  if | "Google Chrome 78." `T.isInfixOf` stdout -> return $ Right 78
     | "Google Chrome 77." `T.isInfixOf` stdout -> return $ Right 77
     | "Google Chrome 76." `T.isInfixOf` stdout -> return $ Right 76
     | "Google Chrome 75." `T.isInfixOf` stdout -> return $ Right 75
     | "Google Chrome 74." `T.isInfixOf` stdout -> return $ Right 74
     | "Google Chrome 73." `T.isInfixOf` stdout -> return $ Right 73
     | "Google Chrome 72." `T.isInfixOf` stdout -> return $ Right 72
     | "Google Chrome 71." `T.isInfixOf` stdout -> return $ Right 71
     | "Google Chrome 70." `T.isInfixOf` stdout -> return $ Right 70
     | "Google Chrome 69." `T.isInfixOf` stdout -> return $ Right 69
     | "Google Chrome 68." `T.isInfixOf` stdout -> return $ Right 68
     | "Google Chrome 67." `T.isInfixOf` stdout -> return $ Right 67
     | otherwise -> return $ Left [i|Couldn't determine Chrome version from string '#{stdout}'|]
