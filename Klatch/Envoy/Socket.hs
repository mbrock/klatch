module Klatch.Envoy.Socket where

import Control.Applicative          ((<$>))
import Control.Concurrent.Async     (concurrently, async)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan)
import Control.Concurrent.STM.TVar  (readTVar, modifyTVar)
import Control.Monad                (void)
import Control.Monad.Error          (catchError)
import Network.Simple.TCP           (Socket, connect)
import Pipes                        (for, runEffect)
import Pipes.Concurrent             (Output, send)

import qualified Data.Map as Map

import Klatch.Envoy.JSON ()
import Klatch.Envoy.Queue
import Klatch.Envoy.Types
import Klatch.Util

handleConnect :: Fleet -> TChan Event -> String -> String -> String -> IO ()
handleConnect fleet channel name host port =
  void . async . flip catchError (writeException name channel) $
    connect host port $ \(socket, _) ->
      onConnect fleet channel socket name host port

handleSend :: Fleet -> TChan Event -> String -> String -> IO ()
handleSend fleet channel name line =
  Map.lookup name <$> atomically (readTVar fleet) >>=
    maybe (writeError name channel "No such server") (flip sendTo line)

addEnvoy :: Fleet -> String -> Output String -> IO ()
addEnvoy m name o = atomically . modifyTVar m . Map.insert name . Envoy $ f
  where f x = do True <- atomically $ send o (x ++ "\n")
                 return ()

receiveLines :: String -> Socket -> TChan Event -> IO ()
receiveLines name socket channel = runEffect $ do
  for (socketLines socket) $ writeEvent channel . Received name
  writeError name channel "Connection closed"

onConnect :: Fleet -> TChan Event -> Socket -> String
          -> String -> String -> IO ()
onConnect fleet channel socket name host port = do
  writeEvent channel $ Connected name host port
  flip catchError (writeException name channel) . void $ concurrently
    (receiveLines name socket channel)
    (addEnvoy fleet name `outputtingTo` writeToSocket socket)
