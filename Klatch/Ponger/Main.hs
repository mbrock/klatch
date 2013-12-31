{-# LANGUAGE ScopedTypeVariables, ViewPatterns, OverloadedStrings #-}

module Main where

import Control.Monad
import Network.IRC.ByteString.Parser
import Pipes

import Klatch.Common.AMQP
import Klatch.Common.Types
import Klatch.Common.Util
import Klatch.Envoy.Queue

main :: IO ()
main = do
  newline
  writeLog "Warming up for some ping pong."

  (amqp, _) <- startAmqp (PluginRole "ponger")

  runEffect $
    readFrom amqp >-> silentDecoder >-> ponger >-> encoder
                  >-> loggingWrites >-> writeTo amqp

ponger :: Pipe Event Command IO ()
ponger = forever $ do
  x <- await
  case payload x of
    LineReceived name line ->
      case parseIRCLine line of
        Just (pong -> Just x') ->
          yield $ LineSend name (toIRCLine x')
        _ -> return ()
    _ ->
      return ()

pong :: IRCMsg -> Maybe IRCMsg
pong x
  | msgCmd x == toUTF8 "PING" =
      Just $ IRCMsg { msgPrefix = Nothing
                    , msgCmd    = toUTF8 "PONG"
                    , msgParams = []
                    , msgTrail  = msgTrail x }
pong _ = Nothing
