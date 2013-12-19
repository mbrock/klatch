{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Klatch.Envoy.JSON () where

import Klatch.Envoy.Types

import Control.Applicative
import Data.Aeson             hiding (Error)
import Data.Aeson.TH
import Data.Text                     (Text)
import Network.IRC.ByteString.Parser (IRCMsg (..), UserInfo (..))

$(deriveJSON defaultOptions ''Command)
$(deriveJSON defaultOptions ''Event)
$(deriveJSON defaultOptions ''EventWithMetadata)
$(deriveJSON defaultOptions ''UserInfo)
$(deriveJSON defaultOptions ''IRCMsg)

-- instance FromJSON Command where
--   parseJSON (Object v) =
--     do command <- v .: "command"
--        case command :: Text of
--          "connect" ->
--            Connect <$> v .: "name" <*> v .: "host" <*> v .: "port"
--          "send" ->
--            Send <$> v .: "name" <*> v .: "line"
--          "ping" ->
--            return Ping
--          _ ->
--            return $ Unknown (Just command)
--   parseJSON _ = return $ Unknown Nothing

-- instance ToJSON Command where
--   toJSON (Connect name host port) =
--     object [ "command" .= ("connect" :: Text)
--            , "name" .= name
--            , "host" .= host
--            , "port" .= port ]
--   toJSON (Send name line) =
--     object [ "command" .= ("send" :: Text)
--            , "name" .= name
--            , "line" .= line ]
--   toJSON Ping =
--     object [ "command" .= ("ping" :: Text) ]

-- instance FromJSON a => FromJSON (EventWithMetadata a) where
--   parseJSON (Object v) =
--     do event <- v .: "event"
--        withMetadata $
--          case event :: String of
--            "connected" ->
--              Connected <$> v .: "name" <*> v .: "host" <*> v .: "port"
--            "received" ->
--              Received <$> v .: "name" <*> v .: "line"
--            "started" ->
--              return Started
--            "stopping" ->
--              return Stopping
--            "pong" ->
--              Pong <$> v .: "count"
--            "error" ->
--              Error <$> v .: "name" <*> v .: "description"
--    where
--      withMetadata x = EventWithMetadata <$>
--        x <*> v .: "time" <*> v .: "version"

-- instance ToJSON a => ToJSON (EventWithMetadata a) where
--   toJSON (EventWithMetadata (Connected name host port) t v) =
--     object [ "event"                .= ("connected" :: Text)
--            , "time"                 .= t
--            , "version"              .= v
--            , "name"                 .= name
--            , "host"                 .= host
--            , "port"                 .= port ]
--   toJSON (EventWithMetadata (Received name line) t v) =
--     object [ "event"                .= ("received" :: Text)
--            , "time"                 .= t
--            , "version"              .= v
--            , "name"                 .= name
--            , "line"                 .= line ]
--   toJSON (EventWithMetadata (Started) t v) =
--     object [ "event"                .= ("started" :: Text)
--            , "time"                 .= t
--            , "version"              .= v ]
--   toJSON (EventWithMetadata (Stopping) t v) =
--     object [ "event"                .= ("stopping" :: Text)
--            , "time"                 .= t
--            , "version"              .= v ]
--   toJSON (EventWithMetadata (Pong n) t v) =
--     object [ "event"                .= ("pong" :: Text)
--            , "count"                .= n
--            , "time"                 .= t
--            , "version"              .= v ]
--   toJSON (EventWithMetadata (Error name description) t v) =
--     object [ "event"                .= ("error" :: Text)
--            , "time"                 .= t
--            , "version"              .= v
--            , "name"                 .= name
--            , "description"          .= description ]

-- instance ToJSON IRCMsg where
--   toJSON 
