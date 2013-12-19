{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Monad                (forever, when, mplus)
import Control.Monad.IO.Class       (liftIO)
import Data.Aeson                   (Value)
import Data.Text                    (Text)
import Pipes                        (Pipe, runEffect, (>->), cat,
                                     for, await, yield, each)

import Klatch.Embassy.FileLog
import Klatch.Envoy.AMQP
import Klatch.Envoy.Queue
import Klatch.Envoy.JSON ()
import Klatch.Envoy.Types (EventWithMetadata (..), Event (..), Command (..))
import Klatch.Util

main :: IO ()
main = do
  newline
  writeLog "Your luxurious embassy is being arranged for visitors."

  startFileLog $ \fileLog (olds :: [EventWithMetadata]) -> do
    when (not $ null olds) $
      writeLog $ "Perusing the embassy archives, " ++ show (length olds)
                 ++ " past moments are recalled..."
      writeLog $ "The archives go back to " ++
                 (bolded . formatTimestamp . eventTimestamp $ head olds) ++ "."

    (amqp, _) <- startAmqp EmbassyRole

    onCtrlC $ do
      writeLog $ concat [ bolded "Stopping.\n\n"
                        , "  Please await your envoys' safe homecoming.\n"
                        , "  To quit immediately, hit Ctrl-C again." ]

    commandQueue <- newTChanIO

    runEffectsConcurrently
       ((each olds `mplus` (readFrom amqp >-> decoder >-> skipNothings))
        >-> doHandshake commandQueue
        >-> loggingIrcMsgs
        >-> into (writeToLog fileLog))
       (contents commandQueue
        >-> encoder
        >-> loggingWrites
        >-> writeTo amqp)

eventDecoder :: Pipe Text (Maybe EventWithMetadata) IO ()
eventDecoder = decoder

doHandshake :: TChan Command -> Pipe EventWithMetadata EventWithMetadata IO ()
doHandshake commandQueue = do
  liftIO . atomically $ writeTChan commandQueue Ping

  writeLog "Waiting for the envoys to report.  Going through old mail..."

  awaitOnce $ \x -> case eventData x of
                      Pong n ->
                        Just . writeLog $
                          "Okay.  The envoys are connected to "
                          ++ plural n "server" ++ "."
                      _ -> Nothing

  continue

loggingIrcMsgs :: Pipe EventWithMetadata EventWithMetadata IO ()
loggingIrcMsgs = forever $ do
  x <- await

  case eventData x of
    Received name line ->
      writeLog $ bolded "IRC: " ++ show (parseIRCLine line)
    _ ->
      return ()

  yield x
