{-# LANGUAGE OverloadedStrings #-}

module Klatch.Envoy.JSON () where

import Klatch.Envoy.Types

import Control.Applicative
import Data.Aeson hiding (Error)

instance FromJSON Command where
  parseJSON (Object v) =
    do command <- v .: "command"
       case command :: String of
         "connect" ->
           Connect <$> v .: "name" <*> v .: "host" <*> v .: "port"
         "send" ->
           Send <$> v .: "name" <*> v .: "line"
         _ ->
           return $ Unknown (Just command)
  parseJSON _ = return $ Unknown Nothing

instance ToJSON Event where
  toJSON (Connected name host port (t, v)) =
    object [ "event"                .= ("connected" :: String)
           , "time"                 .= t
           , "version"              .= v
           , "name"                 .= name
           , "host"                 .= host
           , "port"                 .= port ]
  toJSON (Received name line (t, v)) =
    object [ "event"                .= ("received" :: String)
           , "time"                 .= t
           , "version"              .= v
           , "name"                 .= name
           , "line"                 .= line ]
  toJSON (Started (t, v)) =
    object [ "event"                .= ("started" :: String)
           , "time"                 .= t
           , "version"              .= v ]
  toJSON (Stopping (t, v)) =
    object [ "event"                .= ("stopping" :: String)
           , "time"                 .= t
           , "version"              .= v ]
  toJSON (Error name description (t, v)) =
    object [ "event"                .= ("error" :: String)
           , "time"                 .= t
           , "version"              .= v
           , "name"                 .= name
           , "description"          .= description ]
