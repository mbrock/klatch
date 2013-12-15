{-# LANGUAGE DeriveGeneric #-}

module Klatch.Envoy.Types where

import Control.Concurrent.STM.TVar (TVar)
import Data.Map (Map)
import GHC.Generics (Generic)

envoyVersion :: Int
envoyVersion = 1

type Timestamp = Int

data Command = Connect  String String String
             | Send     String String
             | Unknown  (Maybe String)
               deriving (Eq, Show, Generic)

type EventMetadata = (Timestamp, Int)

data Event = Connected  String String String EventMetadata
           | Received   String String        EventMetadata
           | Error      String String        EventMetadata
           | Started                         EventMetadata
           | Stopping                        EventMetadata
             deriving (Eq, Show, Generic)

newtype Envoy = Envoy { sendTo :: String -> IO () }

type Fleet = TVar (Map String Envoy)
