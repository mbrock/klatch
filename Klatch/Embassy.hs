{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Monad                (forever, when)
import Control.Monad.IO.Class       (liftIO)
import Data.Aeson                   (Value)
import Data.Text                    (Text)
import Network.IRC.ByteString.Parser
import Pipes                        (Pipe, runEffect, (>->), cat,
                                     for, await, yield)

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
    writeLog $ "Perusing the embassy archives, " ++ show (length olds)
               ++ " past moments are recalled..."

    when (not $ null olds) $
      writeLog $ "The archives go back to " ++
                 (bolded . formatTimestamp . eventTimestamp $ last olds) ++ "."

    (amqp, _) <- startAmqp EmbassyRole

    onCtrlC $ do
      writeLog $ concat [ bolded "Stopping.\n\n"
                        , "  Please await your envoys' safe homecoming.\n"
                        , "  To quit immediately, hit Ctrl-C again." ]

    commandQueue <- newTChanIO

    runEffectsConcurrently
       (readFrom amqp
        >-> (decoder :: Pipe Text (Maybe EventWithMetadata) IO ())
        >-> loggingReads
        >-> skipNothings
        >-> doHandshake commandQueue
        >-> into (writeToLog fileLog))
       (contents commandQueue
        >-> encoder
        >-> loggingWrites
        >-> writeTo amqp)

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
