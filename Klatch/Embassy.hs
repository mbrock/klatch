{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Monad                (forever)
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
import Klatch.Envoy.Types (Event (..), Command (..))
import Klatch.Util

main :: IO ()
main = do
  newline
  writeLog "Your luxurious embassy is being arranged for visitors."

  startFileLog $ \(fileLog :: FileLog Event) olds -> do
    writeLog $ "Perusing the embassy archives, " ++ show (length olds)
               ++ " past moments are recalled..."

    (amqp, _) <- startAmqp EmbassyRole

    onCtrlC $ do
      writeLog $ concat [ bolded "Stopping.\n\n"
                        , "  Please await your envoys' safe homecoming.\n"
                        , "  To quit immediately, hit Ctrl-C again." ]

    commandQueue <- newTChanIO

    runEffectsConcurrently
       (readFrom amqp
        >-> (decoder :: Pipe Text (Maybe Event) IO ())
        >-> loggingReads
        >-> skipNothings
        >-> doHandshake commandQueue
        >-> into (writeToLog fileLog))
       (contents commandQueue
        >-> encoder
        >-> writeTo amqp)

doHandshake :: TChan Command -> Pipe Event Event IO ()
doHandshake commandQueue = do
  writeLog $ "Rabbit, please see if you can find the envoys."
  liftIO . atomically $ writeTChan commandQueue Ping

  awaitOnce $ \x -> case x of
                      Pong n _ ->
                        Just . writeLog $
                          "Okay. The envoys are connected to "
                          ++ plural n "server" ++ "."
                      _ -> Nothing

  continue
