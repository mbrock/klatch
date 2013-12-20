{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Klatch.Common.Types where

import Control.Concurrent.STM.TVar   (TVar)
import Data.Aeson.TH
import Data.Map                      (Map)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
import Network.IRC.ByteString.Parser (IRCMsg, UserInfo)
import Prelude                hiding (sequence)

envoyVersion :: Int
envoyVersion = 1

type Timestamp = Int
type EventID   = Int

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
             | Replaying  Int
             | Streaming
             | Pong       Int
               deriving (Eq, Show, Generic)

data EventWithMetadata d i = EventWithMetadata
  { payload   :: Event d
  , timestamp :: Timestamp
  , version   :: Int
  , sequence  :: i }
    deriving (Eq, Show, Generic)

type ParsedEvent = EventWithMetadata IRCMsg EventID
type RawEvent    = EventWithMetadata Text   ()

newtype Envoy = Envoy { sendTo :: Text -> IO () }

type Fleet = TVar (Map Text Envoy)

$(deriveJSON defaultOptions ''Command)
$(deriveJSON defaultOptions ''Event)
$(deriveJSON defaultOptions ''EventWithMetadata)
$(deriveJSON defaultOptions ''UserInfo)
$(deriveJSON defaultOptions ''IRCMsg)

metaevent :: Event IRCMsg -> ParsedEvent
metaevent p = EventWithMetadata {
                timestamp = 0,
                sequence  = -1,
                version   = envoyVersion,
                payload   = p }

isMetaevent :: Event a -> Bool
isMetaevent (Replaying _) = True
isMetaevent Streaming     = True
isMetaevent _             = False
