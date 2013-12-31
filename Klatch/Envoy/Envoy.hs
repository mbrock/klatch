{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Applicative          ((<$>), (<*>))
import Control.Concurrent.STM.TChan (newTChanIO, TChan)
import Control.Concurrent.STM.TVar  (newTVarIO)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Pipes                        (Consumer, cat, for, (>->))

import qualified Data.Map  as Map

import Klatch.Common.AMQP   (Role (EnvoyRole), startAmqp)
import Klatch.Envoy.Queue  (writeTo, readFrom, writeEvent, writeError)
import Klatch.Envoy.Socket (handleConnect, handleSend)
import Klatch.Common.Types
import Klatch.Common.Util  (runEffectsConcurrently, contents, newline,
                            loggingWrites, loggingReads, encoder, decoder,
                            onCtrlC, writeLog, bolded)

main :: IO ()
main = do
  newline
  writeLog "Your illustrious fleet of envoys is setting up."

  (amqp, _) <- startAmqp EnvoyRole
  state@(_, channel) <- initialize

  onCtrlC $ do
    writeLog $ concat [ bolded "Stopping.\n\n"
                      , "  Please await a proper disconnect.\n"
                      , "  To quit immediately, hit Ctrl-C again." ]

  runEffectsConcurrently
    (contents channel >-> loggingWrites >-> encoder      >-> writeTo amqp)
    (readFrom amqp    >-> decoder       >-> loggingReads >-> handler state)

type State = (Fleet, TChan Event)

initialize :: IO State
initialize = (,) <$> (newTVarIO Map.empty) <*> newTChanIO

handler :: State -> Consumer (Maybe Command) IO ()
handler s = for cat (liftIO . handle s)

handle :: State -> Maybe Command -> IO ()
handle (_, c) Nothing    = writeError "" c 0 "Parse error"
handle (f, c) (Just cmd) =
  case cmd of
    SocketStart name host port -> handleConnect f c name host port
    LineSend name line         -> handleSend f c name line
    EventRecord v              -> handleRecord c v
    IRCSend _ _ ->
      writeError "" c 0 "Envoy can't handle this"

handleRecord :: TChan Event -> Payload -> IO ()
handleRecord c p = writeEvent c 0 p
