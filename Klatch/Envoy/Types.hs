{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Klatch.Envoy.Types where

import Control.Concurrent.STM.TVar   (TVar)
import Data.Aeson.TH
import Data.Map                      (Map)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
import Network.IRC.ByteString.Parser (IRCMsg, UserInfo)

envoyVersion :: Int
envoyVersion = 1

type Timestamp = Int

data Command = Connect  Text Text Text
             | Send     Text Text
             | Ping
             | Unknown  (Maybe Text)
               deriving (Eq, Show, Generic)

type EventMetadata = (Timestamp, Int)

data Event a = Connected  Text Text Text
             | Received   Text a
             | Error      Text Text
             | Started
             | Stopping
             | Pong       Int
               deriving (Eq, Show, Generic)

data EventWithMetadata a = EventWithMetadata
  { eventData :: Event a
  , eventTimestamp :: Timestamp
  , version :: Int }
    deriving (Eq, Show, Generic)

type ParsedEvent = EventWithMetadata IRCMsg
type RawEvent    = EventWithMetadata Text

newtype Envoy = Envoy { sendTo :: Text -> IO () }

type Fleet = TVar (Map Text Envoy)

$(deriveJSON defaultOptions ''Command)
$(deriveJSON defaultOptions ''Event)
$(deriveJSON defaultOptions ''EventWithMetadata)
$(deriveJSON defaultOptions ''UserInfo)
$(deriveJSON defaultOptions ''IRCMsg)
