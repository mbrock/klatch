{-# LANGUAGE DeriveGeneric, TemplateHaskell, OverloadedStrings,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Klatch.Common.Types where

import Control.Applicative
import Control.Concurrent.STM.TVar   (TVar)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map                      (Map)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
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

data Command = SocketStartConnecting ServerName HostName Port
             | LineSend ServerName Text
             | IRCSend ServerName IRCMsg
             | EventRecord Event
               deriving (Eq, Show)

oneOf :: MonadPlus m => [a -> m b] -> a -> m b
oneOf fs x = msum (map ($ x) fs)

(==>) :: FromJSON a => Text -> (a -> Parser b) -> Object -> Parser b
(==>) s f x = x .: s >>= \y -> f y

instance FromJSON Event where
  parseJSON (Object v) =
    Event <$> v .: "timestamp" <*> v .: "sequence" <*> flip oneOf v [
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

  parseJSON _ = fail "Unrecognizable event"

parseIrcMsg :: Object -> Parser IRCMsg
parseIrcMsg _ =
  return undefined -- ugh

-- data Command = Connect         Text Text Text
--              | Send            Text Text
--              | Ping
--              | SaveClientEvent Text Text
--              | Unknown         (Maybe Text)
--                deriving (Eq, Show, Generic)

-- type EventMetadata = (Timestamp, Int)

-- data Event a = Connected   Text Text Text
--              | Received    Text a
--              | Error       Text Text
--              | Started
--              | Stopping
--              | Replaying   Int
--              | Streaming
--              | Pong        Int
--              | ClientEvent Text Text
--                deriving (Eq, Show, Generic)

-- data EventWithMetadata d i = EventWithMetadata
--   { payload   :: Event d
--   , timestamp :: Timestamp
--   , version   :: Int
--   , sequence  :: i }
--     deriving (Eq, Show, Generic)

-- type ParsedEvent = EventWithMetadata IRCMsg EventID
-- type RawEvent    = EventWithMetadata Text   ()

-- newtype Envoy = Envoy { sendTo :: Text -> IO () }

-- type Fleet = TVar (Map Text Envoy)

-- $(deriveJSON defaultOptions ''Command)
-- $(deriveJSON defaultOptions ''Event)
-- $(deriveJSON defaultOptions ''EventWithMetadata)
-- $(deriveJSON defaultOptions ''UserInfo)
-- $(deriveJSON defaultOptions ''IRCMsg)

-- metaevent :: Event IRCMsg -> ParsedEvent
-- metaevent p = EventWithMetadata {
--                 timestamp = 0,
--                 sequence  = -1,
--                 version   = envoyVersion,
--                 payload   = p }

-- isMetaevent :: Event a -> Bool
-- isMetaevent (Replaying _) = True
-- isMetaevent Streaming     = True
-- isMetaevent _             = False
