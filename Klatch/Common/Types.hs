{-# LANGUAGE DeriveGeneric, TemplateHaskell, OverloadedStrings,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Klatch.Common.Types where

import Control.Applicative
import Control.Concurrent.STM.TVar   (TVar)
import Control.Monad                 (MonadPlus, msum)
import Data.Aeson
import Data.Aeson.Types              (Parser)
import Data.Map                      (Map)
import Data.Text                     (Text)
import Network.IRC.ByteString.Parser (IRCMsg, UserInfo)
import Prelude                hiding (sequence)

envoyVersion :: Int
envoyVersion = 1

type Timestamp  = Int
type Sequence   = Int
type ServerName = Text
type HostName   = Text
type Port       = Int
type Reason     = Text
type Line       = Text
type Protocol   = Text

data Event = Event { timestamp :: Timestamp
                   , sequence :: Sequence
                   , payload :: Payload }
             deriving (Eq, Show)

data Payload = MetaReplaying Int
             | MetaStreaming

             | SocketStarted ServerName HostName Port
             | SocketSucceeded ServerName
             | SocketFailed ServerName Reason
             | SocketError ServerName Reason
             | SocketEndOfFile ServerName

             | LineReceived ServerName Text
             | LineSent ServerName Text

             | IRCReceived ServerName IRCMsg
             | IRCSent ServerName IRCMsg

             | Other Protocol Value
               deriving (Eq, Show)

data Command = SocketStart ServerName HostName Port
             | LineSend ServerName Text
             | IRCSend ServerName IRCMsg
             | EventRecord Payload
               deriving (Eq, Show)

oneOf :: MonadPlus m => [a -> m b] -> a -> m b
oneOf fs x = msum (map ($ x) fs)

infixr 2 ==>

(==>) :: FromJSON a => Text -> (a -> Parser b) -> Object -> Parser b
(==>) s f x = x .: s >>= \y -> f y

instance FromJSON Event where
  parseJSON (Object v) =
    Event <$> v .: "timestamp" <*> v .: "sequence" <*> parseJSON (Object v)
  parseJSON _ = fail "Unrecognizable event"

instance FromJSON Payload where
  parseJSON (Object v) =
    flip oneOf v [
      "meta" ==> oneOf [
        "Replaying" ==> \x -> MetaReplaying <$> x .: "count",
        "Streaming" ==> \(_ :: Object) -> return MetaStreaming ],

      "socket" ==> oneOf [
        "Started"   ==>
           \x -> SocketStarted   <$> x .: "name" <*> x .: "host"
                                 <*> x .: "port",
        "Succeeded" ==>
           \x -> SocketSucceeded <$> x .: "name",
        "Failed"    ==>
           \x -> SocketFailed    <$> x .: "name" <*> x .: "reason",
        "EndOfFile" ==>
           \x -> SocketEndOfFile <$> x .: "name",
        "Error"     ==>
           \x -> SocketError     <$> x .: "name" <*> x .: "reason" ],

      "line" ==> oneOf [
        "Received" ==>
           \x -> LineReceived    <$> x .: "name" <*> x .: "line",
        "Sent"     ==>
           \x -> LineSent        <$> x .: "name" <*> x .: "line" ],

      "irc" ==> oneOf [
        "Received" ==>
           \x -> IRCReceived     <$> x .: "name" <*> parseIrcMsg x,
        "Sent"     ==>
           \x -> IRCSent         <$> x .: "name" <*> parseIrcMsg x ]]

  parseJSON _ = fail "Unrecognizable payload"

parseIrcMsg :: Object -> Parser IRCMsg
parseIrcMsg _ =
  return undefined -- ugh

instance FromJSON Command where
  parseJSON (Object v) =
    flip oneOf v [
      "socket" ==> "Start" ==>
        \x -> SocketStart <$> x .: "name" <*> x .: "host" <*> x .: "port",
      "line" ==> "Send" ==>
        \x -> LineSend <$> x .: "name" <*> x .: "line",
      "irc" ==> "Send" ==>
        \x -> IRCSend <$> x .: "name" <*> parseIrcMsg x,
      "event" ==> "Record" ==>
        \x -> EventRecord <$> parseJSON x ]

newtype Envoy = Envoy { sendTo :: Text -> IO () }

type Fleet = TVar (Map Text Envoy)

metaevent :: Payload -> Event
metaevent p = Event { timestamp = 0, sequence  = -1, payload = p }

isMetaevent :: Payload -> Bool
isMetaevent (MetaReplaying _) = True
isMetaevent MetaStreaming     = True
isMetaevent _                 = False
