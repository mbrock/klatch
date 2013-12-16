{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Applicative          ((<$>), (<*>))
import Control.Concurrent.STM.TChan (TChan, newTChanIO)
import Control.Concurrent.STM.TVar  (newTVarIO)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Pipes                        (Consumer, cat, for, (>->))

import qualified Data.Map  as Map
import qualified Data.Text as T

import Klatch.Envoy.AMQP   (Role (EnvoyRole), startAmqp)
import Klatch.Envoy.JSON   ()
import Klatch.Envoy.Queue  (writeTo, readFrom, writeEvent, writeError)
import Klatch.Envoy.Socket (handleConnect, handleSend)
import Klatch.Envoy.Types
import Klatch.Util         (runEffectsConcurrently, contents, newline,
                            loggingWrites, loggingReads, encoder, decoder,
                            onCtrlC, writeLog, bolded)

main :: IO ()
main = do
  newline
  writeLog "Your illustrious fleet of envoys is setting up."

  (amqp, _) <- startAmqp EnvoyRole
  state@(_, channel) <- initialize

  writeEvent channel Started

  onCtrlC $ do
    writeEvent channel Stopping
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
handle (_, c) Nothing    = writeError "" c "Parse error"
handle (f, c) (Just cmd) =
  case cmd of
    Connect name host port -> handleConnect f c name host port
    Send name line         -> handleSend f c name line
    Unknown (Just s)       -> writeError "" c (T.append "Unknown command " s)
    Unknown Nothing        -> writeError "" c "Unreadable command"

