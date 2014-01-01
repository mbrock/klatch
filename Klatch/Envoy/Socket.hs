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
import Pipes                        (for, runEffect)
import Pipes.Concurrent             (Output, send)

import qualified Data.Map           as Map
import qualified Data.Text          as T
import qualified Network.Simple.TCP as TCP

import Klatch.Envoy.Queue
import Klatch.Common.Types
import Klatch.Common.Util

handleConnect :: Fleet -> TChan Event -> ServerName -> HostName -> Port
              -> IO ()
handleConnect fleet channel name host port =
  void . async . flip catchError (writeException name channel 0) $ do
    connect host port (onConnect fleet channel name)

connect :: HostName -> Port -> (TCP.Socket -> IO ()) -> IO ()
connect (HostName name) (Port port) f =
  TCP.connect (unpack name) (show port) (f . fst)

handleSend :: Fleet -> TChan Event -> ServerName -> Line -> IO ()
handleSend fleet channel name line =
  Map.lookup name <$> atomically (readTVar fleet) >>=
    maybe (writeError name channel 0 "No such server") (flip sendTo line)

addEnvoy :: Fleet -> ServerName -> Output Text -> IO ()
addEnvoy m name o = atomically . modifyTVar m . Map.insert name . Envoy $ f
  where f (Line x) = do True <- atomically $ send o (T.append x "\n")
                        return ()

receiveLines :: ServerName -> TCP.Socket -> TChan Event -> IO ()
receiveLines name socket channel = runEffect $ do
  for (socketLines socket) $ writeEvent channel 0 . LineReceived name
  writeError name channel 0 "Connection closed"

onConnect :: Fleet -> TChan Event -> ServerName -> TCP.Socket -> IO ()
onConnect fleet channel name socket = do
  writeEvent channel 0 $ SocketSucceeded name
  flip catchError (writeException name channel 0) . void $ concurrently
    (receiveLines name socket channel)
    (addEnvoy fleet name `outputtingTo` writeToSocket socket)
