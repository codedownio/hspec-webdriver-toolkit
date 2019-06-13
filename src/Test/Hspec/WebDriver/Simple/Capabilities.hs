{-# LANGUAGE CPP, QuasiQuotes, ScopedTypeVariables, NamedFieldPuns #-}

module Test.Hspec.WebDriver.Simple.Capabilities where

import Control.Concurrent
import Control.Exception
import qualified Control.Exception.Lifted as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Aeson as A
import Data.Convertible
import Data.Default
import qualified Data.HashMap.Strict as HM
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
import Test.Hspec.WebDriver.Simple.Types
import Test.WebDriver
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Capabilities as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W
import Test.WebDriver.Firefox.Profile

chromeCapabilities :: FilePath -> Capabilities
chromeCapabilities chromeDataDir =
  def {browser=Chrome Nothing Nothing args [] chromePrefs
      , additionalCaps=[("loggingPrefs", A.object [("browser", "ALL")
                                                  , ("client", "WARNING")
                                                  , ("driver", "WARNING")
                                                  , ("performance", "WARNING")
                                                  , ("server", "WARNING")
                                                  ])]
      }
  where args = ["--verbose"]

headlessChromeCapabilities :: FilePath -> Capabilities
headlessChromeCapabilities chromeDataDir =
  def {browser=Chrome Nothing Nothing args [] chromePrefs
      , additionalCaps=[("loggingPrefs", A.object [("browser", "ALL")])]
      }
  where args = ["--verbose", "--headless"]

chromePrefs :: HM.HashMap T.Text A.Value
chromePrefs = HM.fromList [
  ("prefs", A.object [("profile.default_content_setting_values.automatic_downloads", A.Number 1)
                     , ("profile.content_settings.exceptions.automatic_downloads.*.setting", A.Number 1)
                     , ("download.prompt_for_download", A.Bool False)
                     , ("download.directory_upgrade", A.Bool True)
                     , ("download.default_directory", "/tmp")])
  ]

-- getFirefoxCapabilities :: IO Capabilities
-- getFirefoxCapabilities = do
--   profile <- prepareProfile (addPref "webdriver.log.file" ("/tmp/firefox_console" :: String) defaultProfile)
--   let ffAcceptInsecureCerts = Nothing
--   let ff = Firefox (Just profile) LogAll Nothing ffAcceptInsecureCerts
--   return $ def {browser=ff}
