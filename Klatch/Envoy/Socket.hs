{-# LANGUAGE OverloadedStrings #-}

module Klatch.Envoy.Socket where

import Control.Applicative          ((<$>))
import Control.Concurrent.Async     (concurrently, async)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar  (readTVar, modifyTVar)
import Control.Monad                (void)
import Control.Monad.Error          (catchError)
import Data.Text                    (Text, unpack)
import Network.Simple.TCP           (Socket, connect)
import Pipes                        (for, runEffect)
import Pipes.Concurrent             (Output, send)

import qualified Data.Map  as Map
import qualified Data.Text as T

import Klatch.Envoy.Queue
import Klatch.Common.Types
import Klatch.Common.Util

handleConnect :: Fleet -> TChan Event -> ServerName -> HostName -> Port
              -> IO ()
handleConnect fleet channel name host port =
  void . async . flip catchError (writeException name channel 0) $ do
    writeLog $ "Connecting to "
            ++ bolded (string host ++ ":" ++ show port) ++ " ..."
    connect (unpack host) (show port) $ \(socket, _) ->
      onConnect fleet channel socket name

handleSend :: Fleet -> TChan Event -> Text -> Text -> IO ()
handleSend fleet channel name line =
  Map.lookup name <$> atomically (readTVar fleet) >>=
    maybe (writeError name channel 0 "No such server") (flip sendTo line)

addEnvoy :: Fleet -> Text -> Output Text -> IO ()
addEnvoy m name o = atomically . modifyTVar m . Map.insert name . Envoy $ f
  where f x = do True <- atomically $ send o (T.append x "\n")
                 return ()

receiveLines :: Text -> Socket -> TChan Event -> IO ()
receiveLines name socket channel = runEffect $ do
  for (socketLines socket) $ writeEvent channel 0 . LineReceived name
  writeError name channel 0 "Connection closed"

onConnect :: Fleet -> TChan Event -> Socket -> Text -> IO ()
onConnect fleet channel socket name = do
  writeLog $ "Connected to " ++ bolded (string name) ++ "."
  writeEvent channel 0 $ SocketSucceeded name
  flip catchError (writeException name channel 0) . void $ concurrently
    (receiveLines name socket channel)
    (addEnvoy fleet name `outputtingTo` writeToSocket socket)
