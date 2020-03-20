{-# LANGUAGE MultiWayIf, QuasiQuotes, ViewPatterns #-}

module Test.Hspec.WebDriver.Internal.Websockets (
  isWebsocketEntry
  , formatWebsocketEntry
  , ignoreSocketFailures
  ) where

import qualified Data.Aeson as A
import Data.Convertible
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Test.WebDriver


isWebsocketEntry (LogEntry _ _ (getPerformanceLogMethod -> Just method)) =
  method `elem` ["Network.webSocketFrameSent", "Network.webSocketFrameReceived"]
isWebsocketEntry _ = False

formatWebsocketEntry (LogEntry time level text@(getWebsocketPayload -> Just payload)) = [i|#{arrow}   #{payload} (#{time}, #{level})|]
  where arrow = case getPerformanceLogMethod text of
          Just "Network.webSocketFrameSent" -> "--->"
          Just "Network.webSocketFrameReceived" -> "<---"
          _ -> "????"
formatWebsocketEntry _ = "Error formatting websocket entry"

getPerformanceLogMethod ((A.eitherDecode . convert) -> (Right (A.Object (HM.lookup "message" ->
                                                                        (Just (A.Object (HM.lookup "method" ->
                                                                                         Just (A.String method)))))))) = Just method
getPerformanceLogMethod _ = Nothing

getWebsocketPayload ((A.eitherDecode . convert) ->
                      (Right (A.Object (HM.lookup "message" ->
                                       (Just (A.Object (HM.lookup "params" ->
                                                        (Just (A.Object (HM.lookup "response" ->
                                                                         (Just (A.Object (HM.lookup "payloadData" ->
                                                                                          Just (A.String t)))))))))))))) = Just t
getWebsocketPayload _ = Nothing


ignoreSocketFailures :: LogEntry -> Bool
ignoreSocketFailures (LogEntry {logMsg}) | "connection establishment" `T.isInfixOf` logMsg = False
ignoreSocketFailures (LogEntry {logLevel}) = logLevel == LogSevere
