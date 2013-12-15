module Main where

import Pipes (runEffect, (>->))

import Klatch.Envoy.AMQP
import Klatch.Envoy.Queue
import Klatch.Util

main :: IO ()
main = do
  newline
  writeLog "Your luxurious embassy is being arranged."
  
  (amqp, _) <- startAmqp EmbassyRole
  
  onCtrlC $ do
    writeLog $ concat [ bolded "Stopping.\n\n"
                      , "  Please await your envoys' safe homecoming.\n"
                      , "  To quit immediately, hit Ctrl-C again." ]
  
  runEffect $ readFrom amqp >-> loggingReads >-> ignore
  