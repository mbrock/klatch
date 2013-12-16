{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Pipes (runEffect, (>->))

import Klatch.Embassy.FileLog
import Klatch.Envoy.AMQP
import Klatch.Envoy.Queue
import Klatch.Util

main :: IO ()
main = do
  newline
  writeLog "Your luxurious embassy is being arranged."
  
  startFileLog $ \(fileLog :: FileLog String) olds -> do
    writeLog $ "Replenishing " ++ show (length olds) ++ " events."
    
    (amqp, _) <- startAmqp EmbassyRole
    
    onCtrlC $ do
      writeLog $ concat [ bolded "Stopping.\n\n"
                        , "  Please await your envoys' safe homecoming.\n"
                        , "  To quit immediately, hit Ctrl-C again." ]
    
    runEffect $ readFrom amqp >-> loggingReads >-> into (writeToLog fileLog)
  