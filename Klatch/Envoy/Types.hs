{-# LANGUAGE DeriveGeneric #-}

module Klatch.Envoy.Types where

import Control.Concurrent.STM.TVar (TVar)
import Data.Map                    (Map)
import Data.Text                   (Text)
import GHC.Generics                (Generic)

envoyVersion :: Int
envoyVersion = 1

type Timestamp = Int

data Command = Connect  Text Text Text
             | Send     Text Text
             | Unknown  (Maybe Text)
               deriving (Eq, Show, Generic)

type EventMetadata = (Timestamp, Int)

data Event = Connected  Text Text Text EventMetadata
           | Received   Text Text      EventMetadata
           | Error      Text Text      EventMetadata
           | Started                   EventMetadata
           | Stopping                  EventMetadata
             deriving (Eq, Show, Generic)

newtype Envoy = Envoy { sendTo :: Text -> IO () }

type Fleet = TVar (Map Text Envoy)
