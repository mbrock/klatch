{-# LANGUAGE DeriveGeneric, TemplateHaskell, OverloadedStrings,
             ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Klatch.Common.Types where

-- This module is very dull and repetitive.  Maybe there's a more
-- abstract way of doing this JSON business, but this works well
-- enough.

import Test.QuickCheck hiding ((==>))

import Control.Applicative
import Control.Concurrent.STM.TVar   (TVar)
import Control.Monad                 (MonadPlus, msum)
import Data.Aeson
import Data.Aeson.Types              (Parser)
import Data.String                   (IsString)
import Data.Map                      (Map)
import Data.Text                     (Text)
import Network.IRC.ByteString.Parser (IRCMsg (..), UserInfo (..))
import Prelude                hiding (sequence)

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as E
import qualified Data.HashMap.Strict as HM

envoyVersion :: Int
envoyVersion = 1

newtype Timestamp  = Timestamp Int   deriving (Eq, Show, Arbitrary,
                                               FromJSON, ToJSON,
                                               Num, Integral, Real,
                                               Enum, Ord)
newtype Sequence   = Sequence Int    deriving (Eq, Show, Arbitrary, Num,
                                               FromJSON, ToJSON)
newtype ServerName = ServerName Text deriving (Eq, Show, Arbitrary,
                                               FromJSON, ToJSON, Ord,
                                               IsString)
newtype HostName   = HostName Text   deriving (Eq, Show, Arbitrary,
                                               FromJSON, ToJSON)
newtype Port       = Port Int        deriving (Eq, Show, Arbitrary,
                                               FromJSON, ToJSON)
newtype Reason     = Reason Text     deriving (Eq, Show, Arbitrary,
                                               FromJSON, ToJSON)
newtype Line       = Line Text       deriving (Eq, Show, Arbitrary,
                                               FromJSON, ToJSON)
newtype Protocol   = Protocol Text   deriving (Eq, Show, Arbitrary,
                                               FromJSON, ToJSON)

instance Arbitrary Text where
  arbitrary = elements ["foo", "bar", "baz"]

data Event = Event { timestamp :: Timestamp
                   , sequence :: Sequence
                   , payload :: Payload }
             deriving (Eq, Show)

instance Arbitrary Event where
  arbitrary = Event <$> arbitrary <*> arbitrary <*> arbitrary

data Payload = MetaReplaying Int
             | MetaStreaming

             | SocketStarted ServerName HostName Port
             | SocketSucceeded ServerName
             | SocketFailed ServerName Reason
             | SocketError ServerName Reason
             | SocketEndOfFile ServerName

             | LineReceived ServerName Line
             | LineSent ServerName Line

             | IRCReceived ServerName IRCMsg
             | IRCSent ServerName IRCMsg

             | Other Protocol Value
               deriving (Eq, Show)

instance Arbitrary Payload where
  arbitrary = oneof [
      MetaReplaying <$> arbitrary
    , return MetaStreaming
    , SocketStarted <$> arbitrary <*> arbitrary <*> arbitrary
    , SocketSucceeded <$> arbitrary
    , SocketFailed <$> arbitrary <*> arbitrary
    , SocketError <$> arbitrary <*> arbitrary
    , SocketEndOfFile <$> arbitrary
    , LineReceived <$> arbitrary <*> arbitrary
    , LineSent <$> arbitrary <*> arbitrary
    , IRCReceived <$> arbitrary <*> arbitrary
    , IRCSent <$> arbitrary <*> arbitrary
    , Other <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Value where
  arbitrary = return $ Null

instance Arbitrary IRCMsg where
  arbitrary = IRCMsg <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BS.ByteString where
  arbitrary = fmap E.encodeUtf8 arbitrary

instance Arbitrary UserInfo where
  arbitrary = UserInfo <$> arbitrary <*> arbitrary <*> arbitrary

data Command = SocketStart ServerName HostName Port
             | LineSend ServerName Line
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
        "Streaming" ==> \(_ :: Value) -> return MetaStreaming ],

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
           \x -> IRCSent         <$> x .: "name" <*> parseIrcMsg x ],

      \_ -> Other (Protocol $ protocolName v) <$> v .: protocolName v ]

  parseJSON _ = fail "Unrecognizable payload"

protocolName :: Object -> Text
protocolName v = head (filter (not . boring) (HM.keys v))
  where boring "sequence" = True
        boring "timestamp" = True
        boring _ = False

parseIrcMsg :: Object -> Parser IRCMsg
parseIrcMsg x =
  IRCMsg <$> ("prefix" ==> parsePrefix) x
         <*> x .: "command" <*> x .: "params" <*> x .: "trail"

parsePrefix :: Value -> Parser (Maybe (Either UserInfo BS.ByteString))
parsePrefix (Object x) = flip oneOf x [
  "User" ==>
    \y -> fmap (Just . Left) $
      UserInfo <$> y .: "nick" <*> y .: "name" <*> y .: "host",
  "Server" ==>
    \y -> Just . Right <$> y .: "host",
  (\_ -> fail "Unrecognizable prefix")]
parsePrefix Null = return Nothing
parsePrefix _ = fail "Unrecognizable prefix"

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
  parseJSON _ = fail "Unrecognizable command"

instance ToJSON Event where
  toJSON (Event t s p) =
    object $ ["timestamp" .= t, "sequence" .= s] ++ [payloadAttribute p]

instance ToJSON Payload where
  toJSON p = object [payloadAttribute p]

infixr 2 .==

(.==) :: Text -> (Text, Value) -> (Text, Value)
s .== (k, v) = (s, object [(k, v)])

payloadAttribute :: Payload -> (Text, Value)
payloadAttribute p = case p of
  MetaReplaying a ->
    "meta" .== "Replaying" .= object ["count" .= a]
  MetaStreaming ->
    "meta" .== "Streaming" .= True
  SocketStarted a b c ->
    "socket" .== "Started" .= object ["name" .= a, "host" .= b, "port" .= c]
  SocketSucceeded a ->
    "socket" .== "Succeeded" .= object ["name" .= a]
  SocketFailed a b ->
    "socket" .== "Failed" .= object ["name" .= a, "reason" .= b]
  SocketEndOfFile a ->
    "socket" .== "EndOfFile" .= object ["name" .= a]
  SocketError a b ->
    "socket" .== "Error" .= object ["name" .= a, "reason" .= b]
  LineReceived a b ->
    "line" .== "Received" .= object ["name" .= a, "line" .= b]
  LineSent a b ->
    "line" .== "Sent" .= object ["name" .= a, "line" .= b]
  IRCReceived a b ->
    "irc" .== "Received" .= object ("name" .= a : ircMsgAttrs b)
  IRCSent a b ->
    "irc" .== "Sent" .= object ("name" .= a : ircMsgAttrs b)
  Other (Protocol a) b ->
    a .= b

ircMsgAttrs :: IRCMsg -> [(Text, Value)]
ircMsgAttrs m = [
  "prefix" .= case msgPrefix m of
    Nothing -> Null
    Just (Left x) -> object ["User" .= object ["nick" .= userNick x,
                                               "name" .= userName x,
                                               "host" .= userHost x]]
    Just (Right x) -> object ["Server" .= object ["host" .= x]],
  "command" .= toJSON (msgCmd m),
  "params" .= toJSON (msgParams m),
  "trail" .= toJSON (msgTrail m)]

instance ToJSON Command where
  toJSON p = object [case p of
    SocketStart a b c ->
      "socket" .== "Start" .= object ["name" .= a, "host" .= b, "port" .= c]
    LineSend a b ->
      "line" .== "Send" .= object ["name" .= a, "line" .= b]
    IRCSend a b ->
      "irc" .== "Send" .= object ("name" .= a : ircMsgAttrs b)
    EventRecord a ->
      "event" .== "Record" .= a]

newtype Envoy = Envoy { sendTo :: Line -> IO () }

type Fleet = TVar (Map ServerName Envoy)

metaevent :: Payload -> Event
metaevent p = Event { timestamp = 0, sequence  = -1, payload = p }

isMetaevent :: Payload -> Bool
isMetaevent (MetaReplaying _) = True
isMetaevent MetaStreaming     = True
isMetaevent _                 = False
