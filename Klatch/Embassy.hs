{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Main where

import Control.Concurrent.Async      (async, link)
import Control.Concurrent.STM        (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad                 (forever, when)
import Control.Monad.IO.Class        (liftIO)
import Data.Text                     (Text)
import Pipes                         (Pipe, Producer, (>->),
                                      await, yield, each)
import Prelude                hiding (sequence)

import Klatch.Embassy.FileLog
import Klatch.Envoy.AMQP
import Klatch.Envoy.Queue
import Klatch.Envoy.Types
import Klatch.Util

import qualified Klatch.Embassy.HTTP

type EmbassyState = TVar EventID

main :: IO ()
main = do
  newline
  writeLog "Your luxurious embassy is being arranged for visitors."

  startFileLog $ \fileLog (olds :: [ParsedEvent]) -> do
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

    async (Klatch.Embassy.HTTP.run eventQueue) >>= link

    runEffectsConcurrently
       (contents commandQueue >-> encoder >-> writeTo amqp)
       ((replaying olds
         >> (readFrom amqp >-> decodingIrcMsgs commandQueue state
                           >-> loggingReads
                           >-> handlingPings commandQueue
                           >-> into (writeToLog fileLog)))
        >-> toChannel eventQueue)

replaying :: [ParsedEvent] -> Producer ParsedEvent IO ()
replaying olds = do
  yield . metaevent $ Replaying (length olds)
  each olds
  yield . metaevent $ Streaming

doHandshake :: TChan Command -> Pipe RawEvent RawEvent IO ()
doHandshake commandQueue = do
  liftIO . atomically $ writeTChan commandQueue Ping

  writeLog "Waiting for the envoys to report.  Going through old mail..."

  awaitOnce $ \x -> case payload x of
                      Pong n ->
                        Just . writeLog $
                          "Okay.  The envoys are connected to "
                          ++ plural n "server" ++ "."
                      _ -> Nothing

  continue

decodingIrcMsgs :: TChan Command -> EmbassyState -> Pipe Text ParsedEvent IO ()
decodingIrcMsgs commandQueue state =
  decoder >-> skipNothings >-> doHandshake commandQueue
          >-> forever (decodeIrcMsg state)

decodeIrcMsg :: EmbassyState -> Pipe RawEvent ParsedEvent IO ()
decodeIrcMsg state = do
  x <- await
  e <- case payload x of
         Received name line -> do
           writeLog $ bolded "IRC: " ++ show (parseIRCLine line)
           case parseIRCLine line of
             Just msg -> return $ Just (Received name msg)
             _        -> return Nothing

         Connected a b c -> return $ Just (Connected a b c)
         Error a b       -> return $ Just (Error a b)
         Started         -> return $ Just Started
         Stopping        -> return $ Just Stopping
         Pong a          -> return $ Just (Pong a)

         (isMetaevent -> True) -> return Nothing
         _                     -> error "Someone made a mistake."

  case e of
    Just d -> do
      nextId <- liftIO (nextEventId state)
      yield $ x { payload = d, sequence = nextId }
    Nothing -> return ()

nextEventId :: EmbassyState -> IO EventID
nextEventId state = atomically (modifyTVar state (+ 1) >> readTVar state)

handlingPings :: TChan Command -> Pipe ParsedEvent ParsedEvent IO ()
handlingPings q = forever $ do
  x <- await
  case payload x of
    Received name (pong -> Just x') ->
      liftIO . atomically . writeTChan q . Send name . toIRCLine $ x'
    _ -> return ()

  yield x

