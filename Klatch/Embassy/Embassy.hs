{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Klatch.Embassy.Embassy (main) where

import Control.Concurrent.Async      (async, link)
import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad                 (forever, when)
import Control.Monad.IO.Class        (liftIO)
import Pipes                         (Pipe, Producer, (>->),
                                      await, yield, each)
import Prelude                hiding (sequence)

import Klatch.Embassy.FileLog
import Klatch.Common.AMQP
import Klatch.Envoy.Queue
import Klatch.Common.Types
import Klatch.Common.Util

import qualified Klatch.Embassy.HTTP

type EmbassyState = TVar Sequence

main :: IO ()
main = do
  newline
  writeLog "Your luxurious embassy is being arranged for visitors."

  startFileLog $ \fileLog (olds :: [Event]) -> do
    when (not $ null olds) $ do
      let count        = length olds
          epoch        = timestamp (head olds)
      writeLog $ "Perusing the embassy archives, " ++ show count
                 ++ " past moments are recalled..."
      writeLog $ "The archives go back to " ++
                 bolded (formatTimestamp epoch) ++ "."

    let nextSequence = case olds of
                         [] -> -1
                         _  -> sequence (last olds)

    (amqp, _) <- startAmqp EmbassyRole

    onCtrlC $ do
      writeLog $ concat [ bolded "Stopping.\n\n"
                        , "  Please await your envoys' safe homecoming.\n"
                        , "  To quit immediately, hit Ctrl-C again." ]

    eventQueue   <- newTChanIO
    commandQueue <- newTChanIO
    state        <- newTVarIO nextSequence

    async (Klatch.Embassy.HTTP.run eventQueue commandQueue) >>= link

    runEffectsConcurrently
       (contents commandQueue >-> encoder >-> writeTo amqp)
       ((replaying olds >>
          (readFrom amqp >-> decoder
                         >-> skipNothings
                         >-> into (writeToLog fileLog)))
        >-> forever (decodeIrcMsg state)
        >-> toChannel eventQueue)

replaying :: [Event] -> Producer Event IO ()
replaying olds = do
  yield . metaevent $ MetaReplaying (length olds)
  each olds
  yield . metaevent $ MetaStreaming

decodeIrcMsg :: EmbassyState -> Pipe Event Event IO ()
decodeIrcMsg state = do
  x <- await
  e <- case payload x of
         LineReceived name line -> do
           writeLog $ bolded "IRC: " ++ show (parseIRCLine line)
           case parseIRCLine line of
             Just msg -> return $ Just (IRCReceived name msg)
             _        -> return Nothing
         p -> return $ Just p

  case e of
    Just d -> do
      nextId <- liftIO (nextEventId state)
      yield $ x { payload = d, sequence = nextId }
    Nothing -> return ()

nextEventId :: EmbassyState -> IO Sequence
nextEventId state = atomically (modifyTVar state (+ 1) >> readTVar state)
