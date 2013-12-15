{-# LANGUAGE TupleSections, NamedFieldPuns #-}

module Main where

import Klatch.Util

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

import Pipes (Effect, runEffect, (>->), for)
import qualified Pipes.Prelude as P

import Control.Monad          (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.List              ((\\))

import Control.Concurrent.Async     (Async, concurrently, async, link)
import Control.Concurrent.STM       (STM, atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)

import System.Posix.Env (getEnv)
import System.Exit (exitFailure)

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Options.Applicative.Utils (tabulate)

defaults :: Map String (Maybe String)
defaults =
  Map.fromList [ ("BUSSER_AMQP_HOST"        , Just "127.0.0.1")
               , ("BUSSER_AMQP_VHOST"       , Just "/")
               , ("BUSSER_AMQP_USER"        , Just "guest")
               , ("BUSSER_AMQP_PASSWORD"    , Just "guest")
               , ("BUSSER_AMQP_IN_QUEUE"    , Nothing)
               , ("BUSSER_AMQP_OUT_QUEUE"   , Nothing)
               , ("BUSSER_AMQP_EXCHANGE"    , Nothing)
               , ("BUSSER_AMQP_BINDING_KEY" , Just "busser") ]
  
onlyJusts :: Ord k => Map k (Maybe v) -> Map k v
onlyJusts = Map.mapMaybe id

parameters :: [String]
parameters = Map.keys defaults
  
getParametersAndUsedDefaults :: IO (Map String String, [String])
getParametersAndUsedDefaults = do
  env <- fmap Map.fromList (mapM (\k -> fmap (k,) (getEnv k)) parameters)
  let params = Map.union (onlyJusts env) (onlyJusts defaults)
  return (params, Map.keys $ Map.difference params (onlyJusts env))

getParameters :: IO (Map String String)
getParameters = do
  (params, usedDefaults) <- getParametersAndUsedDefaults
  putStrLn "Using the following parameters:"
  let f (k, v) = (k, v ++ if elem k usedDefaults
                          then " (default)"
                          else "")
  putStrLn . unlines . tabulate . map f $ Map.assocs params
  when (Map.size params < length parameters) $ do
    putStrLn "Missing the following mandatory parameters:"
    printIndentedList 2 (parameters \\ Map.keys params)
    newline
    exitFailure
  return params

data Queue = Queue { input  :: TChan String
                   , output :: String -> STM () }

connect :: Map String String -> IO (Queue, Async ())
connect params = do
  let host       = params ! "BUSSER_AMQP_HOST"
      vhost      = params ! "BUSSER_AMQP_VHOST"
      user       = params ! "BUSSER_AMQP_USER"
      password   = params ! "BUSSER_AMQP_PASSWORD"
      inQueue    = params ! "BUSSER_AMQP_IN_QUEUE"
      outQueue   = params ! "BUSSER_AMQP_OUT_QUEUE"
      exchange   = params ! "BUSSER_AMQP_EXCHANGE"
      bindingKey = params ! "BUSSER_AMQP_BINDING_KEY"

  conn <- openConnection host vhost user password
  chan <- openChannel conn

  declareQueue chan newQueue { queueName = inQueue }
  declareQueue chan newQueue { queueName = outQueue }

  declareExchange chan newExchange { exchangeName = exchange
                                   , exchangeType = "direct" }

  bindQueue chan outQueue exchange bindingKey
  
  queueChan <- newTChanIO

  let report = atomically . writeTChan queueChan . readMsg
      onMsg (msg, env) = report msg >> ackEnv env

  consumeMsgs chan inQueue Ack onMsg

  outputChan <- newTChanIO
  writer <- async . runEffect . for (contents outputChan) $
    liftIO . publishMsg chan exchange bindingKey . makeMsg

  return $ (Queue queueChan (writeTChan outputChan), writer)

readMsg :: Message -> String
readMsg = BL.unpack . msgBody

makeMsg :: String -> Message
makeMsg s = newMsg { msgBody = BL.pack s
                   , msgDeliveryMode = Just Persistent }

readFrom :: Queue -> Effect IO ()
readFrom Queue { input } = contents input >-> P.stdoutLn

writeTo :: Queue -> Effect IO ()
writeTo Queue { output } = for P.stdinLn (liftIO . atomically . output)
                        
main :: IO ()
main = do
  newline >> putStrLn "Starting Busser..." >> newline
  params <- getParameters
  (queue, writer) <- connect params
  runEffectsConcurrently (readFrom queue) (writeTo queue)
  link writer
  exitFailure
