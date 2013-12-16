{-# LANGUAGE TupleSections #-}

module Klatch.Envoy.AMQP (startAmqp, Role (EnvoyRole, EmbassyRole)) where

import Klatch.Envoy.Queue
import Klatch.Params
import Klatch.Util

import Control.Concurrent.Async     (Async, async)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import Control.Monad                (void)
import Control.Monad.IO.Class       (liftIO)
import Data.Map                     ((!))
import Network.AMQP
import Pipes                        (runEffect, for)

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map

data Role = EnvoyRole | EmbassyRole

startAmqp :: Role -> IO (Queue, Async ())
startAmqp role = getAmqpParameters role >>= connect role

defaults :: Role -> ParamSpec
defaults role =
  Map.fromList [ ("ENVOY_AMQP_HOST"        , Just "127.0.0.1")
               , ("ENVOY_AMQP_VHOST"       , Just "/")
               , ("ENVOY_AMQP_USER"        , Just "guest")
               , ("ENVOY_AMQP_PASSWORD"    , Just "guest")
               , ("ENVOY_AMQP_IN_QUEUE"    , Just "envoy-in")
               , ("ENVOY_AMQP_OUT_QUEUE"   , Just "envoy-out")
               , ("ENVOY_AMQP_EXCHANGE"    , Just "klatch")
               , ("ENVOY_AMQP_BINDING_KEY" , Just (getBindingKey role)) ]
  
getBindingKey :: Role -> String
getBindingKey EnvoyRole   = "envoy"
getBindingKey EmbassyRole = "embassy"

getAmqpParameters :: Role -> IO Params
getAmqpParameters r = getParameters "AMQP" (defaults r)

getInQueue :: Role -> Params -> String
getInQueue EnvoyRole   = (! "ENVOY_AMQP_IN_QUEUE")
getInQueue EmbassyRole = (! "ENVOY_AMQP_OUT_QUEUE")

getOutQueue :: Role -> Params -> String
getOutQueue EnvoyRole   = getInQueue EmbassyRole
getOutQueue EmbassyRole = getInQueue EnvoyRole

connect :: Role -> Params -> IO (Queue, Async ())
connect role params = do
  let host       = params ! "ENVOY_AMQP_HOST"
      vhost      = params ! "ENVOY_AMQP_VHOST"
      user       = params ! "ENVOY_AMQP_USER"
      password   = params ! "ENVOY_AMQP_PASSWORD"
      inQueue    = getInQueue role params
      outQueue   = getOutQueue role params
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
makeMsg s = newMsg { msgBody = BL.pack s, msgDeliveryMode = Just Persistent }
