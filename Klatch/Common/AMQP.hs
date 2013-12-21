{-# LANGUAGE TupleSections, OverloadedStrings #-}

module Klatch.Common.AMQP (startAmqp, Role (..)) where

import Klatch.Envoy.Queue
import Klatch.Common.Params
import Klatch.Common.Util

import Control.Concurrent.Async     (Async, async)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (newTChanIO, writeTChan)
import Control.Monad                (void)
import Control.Monad.IO.Class       (liftIO)
import Data.Map                     ((!))
import Data.Text                    (Text, unpack)
import Network.AMQP
import Pipes                        (runEffect, for)

import qualified Data.Text.Lazy.Encoding    as LE
import qualified Data.Text.Lazy             as TL
import qualified Data.Map                   as Map

data Role = EnvoyRole | EmbassyRole | PluginRole Text

startAmqp :: Role -> IO (Queue, Async ())
startAmqp role = getAmqpParameters role >>= connect role

defaults :: Role -> ParamSpec
defaults role =
  Map.fromList [ ("ENVOY_AMQP_HOST"         , Just "127.0.0.1")
               , ("ENVOY_AMQP_VHOST"        , Just "/")
               , ("ENVOY_AMQP_USER"         , Just "guest")
               , ("ENVOY_AMQP_PASSWORD"     , Just "guest")
               , ("ENVOY_AMQP_QUEUE"        , Just (getQueue role))
               , ("ENVOY_AMQP_IN_EXCHANGE"  , Just (getInExchange role))
               , ("ENVOY_AMQP_OUT_EXCHANGE" , Just (getOutExchange role))
               , ("ENVOY_AMQP_BINDING_KEY"  , Just (getBindingKey role)) ]

getQueue :: Role -> Text
getQueue EnvoyRole      = "commands"
getQueue EmbassyRole    = "events"
getQueue (PluginRole s) = s

getBindingKey :: Role -> Text
getBindingKey = const ""

getInExchange :: Role -> Text
getInExchange EnvoyRole      = "envoy"
getInExchange EmbassyRole    = "embassy"
getInExchange (PluginRole _) = "embassy"

getOutExchange :: Role -> Text
getOutExchange EnvoyRole      = "embassy"
getOutExchange EmbassyRole    = "envoy"
getOutExchange (PluginRole _) = "envoy"

inExchangeType :: Role -> Text
inExchangeType EnvoyRole = "direct"
inExchangeType _         = "fanout"

outExchangeType :: Role -> Text
outExchangeType EnvoyRole = "fanout"
outExchangeType _         = "direct"

getAmqpParameters :: Role -> IO Params
getAmqpParameters r = getParameters "AMQP" (defaults r)

connect :: Role -> Params -> IO (Queue, Async ())
connect role params = do
  let host        = params ! "ENVOY_AMQP_HOST"
      vhost       = params ! "ENVOY_AMQP_VHOST"
      user        = params ! "ENVOY_AMQP_USER"
      password    = params ! "ENVOY_AMQP_PASSWORD"
      inExchange  = getInExchange role
      outExchange = getOutExchange role
      queue       = params ! "ENVOY_AMQP_QUEUE"
      bindingKey  = params ! "ENVOY_AMQP_BINDING_KEY"

  conn <- openConnection (unpack host) vhost user password
  chan <- openChannel conn

  void $ declareQueue chan newQueue { queueName = queue }
  void $ declareExchange chan newExchange {
                        exchangeName = inExchange
                      , exchangeType = inExchangeType role }
  void $ declareExchange chan newExchange {
                        exchangeName = outExchange
                      , exchangeType = outExchangeType role }

  bindQueue chan queue inExchange bindingKey

  queueChan <- newTChanIO

  let report = atomically . writeTChan queueChan . readMsg
      onMsg (msg, env) = report msg >> ackEnv env

  void $ consumeMsgs chan queue Ack onMsg

  outputChan <- newTChanIO
  writer <- async . runEffect . for (contents outputChan) $
    liftIO . publishMsg chan outExchange bindingKey . makeMsg

  return $ (Queue queueChan (writeTChan outputChan), writer)

readMsg :: Message -> Text
readMsg = TL.toStrict . LE.decodeUtf8 . msgBody

makeMsg :: Text -> Message
makeMsg t = newMsg { msgBody = s, msgDeliveryMode = Just Persistent }
  where s = LE.encodeUtf8 (TL.fromStrict t)
