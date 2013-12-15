{-# LANGUAGE TupleSections, NamedFieldPuns #-}

module Klatch.Busser (
  getParameters,
  Queue,
  connect,
  readFrom,
  writeTo
) where

import Klatch.Util

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL

import Pipes (Producer, Consumer, runEffect, for, cat)

import Control.Monad          (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.List              ((\\))

import Control.Concurrent.Async     (Async, async)
import Control.Concurrent.STM       (STM, atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)

import System.Posix.Env (getEnv)
import System.Exit (exitFailure)

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Options.Applicative.Utils (tabulate)

defaults :: Map String (Maybe String)
defaults =
  Map.fromList [ ("ENVOY_AMQP_HOST"        , Just "127.0.0.1")
               , ("ENVOY_AMQP_VHOST"       , Just "/")
               , ("ENVOY_AMQP_USER"        , Just "guest")
               , ("ENVOY_AMQP_PASSWORD"    , Just "guest")
               , ("ENVOY_AMQP_IN_QUEUE"    , Nothing)
               , ("ENVOY_AMQP_OUT_QUEUE"   , Nothing)
               , ("ENVOY_AMQP_EXCHANGE"    , Nothing)
               , ("ENVOY_AMQP_BINDING_KEY" , Just "envoy") ]
  
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
  let host       = params ! "ENVOY_AMQP_HOST"
      vhost      = params ! "ENVOY_AMQP_VHOST"
      user       = params ! "ENVOY_AMQP_USER"
      password   = params ! "ENVOY_AMQP_PASSWORD"
      inQueue    = params ! "ENVOY_AMQP_IN_QUEUE"
      outQueue   = params ! "ENVOY_AMQP_OUT_QUEUE"
      exchange   = params ! "ENVOY_AMQP_EXCHANGE"
      bindingKey = params ! "ENVOY_AMQP_BINDING_KEY"

  conn <- openConnection host vhost user password
  chan <- openChannel conn

  void $ declareQueue chan newQueue { queueName = inQueue }
  void $ declareQueue chan newQueue { queueName = outQueue }

  void $ declareExchange chan newExchange { exchangeName = exchange
                                          , exchangeType = "direct" }

  bindQueue chan outQueue exchange bindingKey
  
  queueChan <- newTChanIO

  let report = atomically . writeTChan queueChan . readMsg
      onMsg (msg, env) = report msg >> ackEnv env

  void $ consumeMsgs chan inQueue Ack onMsg

  outputChan <- newTChanIO
  writer <- async . runEffect . for (contents outputChan) $
    liftIO . publishMsg chan exchange bindingKey . makeMsg

  return $ (Queue queueChan (writeTChan outputChan), writer)

readMsg :: Message -> String
readMsg = BL.unpack . msgBody

makeMsg :: String -> Message
makeMsg s = newMsg { msgBody = BL.pack s
                   , msgDeliveryMode = Just Persistent }

readFrom :: Queue -> Producer String IO ()
readFrom Queue { input } = contents input

writeTo :: Queue -> Consumer String IO ()
writeTo Queue { output } = for cat (liftIO . atomically . output)
