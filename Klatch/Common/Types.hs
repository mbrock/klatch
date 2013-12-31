{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Klatch.Common.Types where

import Control.Applicative
import Control.Concurrent.STM.TVar   (TVar)
import Data.Aeson
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

data Payload = MetaReplaying Int
             | MetaStreaming
             | SocketConnectionStarted ServerName HostName Port
             | SocketConnectionSucceeded ServerName
             | SocketConnectionFailed ServerName Reason
             | LineReceived ServerName Text
             | LineSent ServerName Text
             | IRCReceived ServerName IRCMsg
             | IRCSent ServerName IRCMsg
             | Other Protocol Value
               deriving (Eq, Ord, Show, Read)

data Command = SocketStartConnecting ServerName HostName Port
             | LineSend ServerName Text
             | IRCSend ServerName IRCMsg
             | EventRecord Event
               deriving (Eq, Ord, Show, Read)

instance FromJSON Event where
  parseJSON (Object v) =
    Event <$> v .: "timestamp" <*> v .: "sequence" <*> parsePayload v

oneOf fs x = mappend (map ($ x) fs)
asOneOf s fs x =
  do y <- x .: s
     mappend (map (\(k, f) -> do z <- y .: k
                                 f z) fs)

-- This follows the definition in <events.txt>.
parsePayload = oneOf [
  "meta" `asOneOf` [
    ("Replaying", \x -> MetaReplaying <$> x .: "count"),
    ("Streaming", const $ return MetaStreaming)],
  "socket" `asOneOf` [
    ("ConnectionStarted", \x ->
       SocketConnectionStarted <$> x .: "name"
                               <*> x .: "host"
                               <*> x .: "port"),
    ("ConnectionSucceeded", \x ->
       SocketConnectionSucceeded <$> x .: "name")
    ("ConnectionFailed", \x ->
       SocketConnectionFailed <$> x .: "name" <*> x .: "reason"),
    ("EndOfFile", \x ->
       SocketEndOfFile <$> x .: "name"),
    ("ConnectionError", \x ->
       SocketConnectionError <$> x .: "name" <*> x .: "reason") ],
  "line" `asOneOf` [
    ("Received", \x ->
       LineReceived <$> x .: "name" <*> x .: "line"),
    ("Sent", \x ->
       LineSent <$> x .: "name" <*> x .: "line") ],
  "irc" `asOneOf` [
    ("Received", \x ->
       IRCReceived <$> x .: "name" <*> parseIrcMsg x),
    ("Sent", \x ->
       IRCSent <$> x .: "name" <*> parseIrcMsg x)
  ]
]

parseIrcMsg x =
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
