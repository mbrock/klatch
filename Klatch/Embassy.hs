{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson (Value)
import Data.Text (Text)
import Pipes (Pipe, runEffect, (>->))
import Network.IRC.ByteString.Parser

import Klatch.Embassy.FileLog
import Klatch.Envoy.AMQP
import Klatch.Envoy.Queue
import Klatch.Envoy.JSON ()
import Klatch.Envoy.Types (Event)
import Klatch.Util

main :: IO ()
main = do
  newline
  writeLog "Your luxurious embassy is being arranged."

  startFileLog $ \(fileLog :: FileLog Event) olds -> do
    writeLog $ "Replenishing " ++ show (length olds) ++ " events."

    (amqp, _) <- startAmqp EmbassyRole

    onCtrlC $ do
      writeLog $ concat [ bolded "Stopping.\n\n"
                        , "  Please await your envoys' safe homecoming.\n"
                        , "  To quit immediately, hit Ctrl-C again." ]

    runEffect $ readFrom amqp
       >-> (decoder :: Pipe Text (Maybe Event) IO ())
       >-> loggingReads
       >-> skipNothings
       >-> into (writeToLog fileLog)
