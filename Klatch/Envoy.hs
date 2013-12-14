{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Klatch.Envoy where

import Klatch.Util

import Control.Applicative
import Control.Exception (IOException)
import Control.Monad (void)
import Control.Monad.Error (catchError)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Data.Aeson (FromJSON, ToJSON, Value (..),
                   (.:), (.=), object, parseJSON, toJSON)
import GHC.Generics (Generic)

import           Data.Map   (Map)
import qualified Data.Map as Map

import Pipes
import Pipes.Concurrent

import Network.Simple.TCP (Socket, connect)

type Timestamp = Int

data Command = Connect String String String
             | Send String String
             | Unknown (Maybe String)
               deriving (Eq, Show, Generic)

data Event = Connected String String String Timestamp
           | Received String String Timestamp
           | Error String String Timestamp
             deriving (Eq, Show, Generic)

newtype Envoy = Envoy { sendTo :: String -> IO () }

type Fleet = TVar (Map String Envoy)

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
  toJSON (Connected name host port t) =
    object [ "event" .= ("connected" :: String)
           , "time"  .= t
           , "name"  .= name
           , "host"  .= host
           , "port"  .= port ]
  toJSON (Received name line t) =
    object [ "event" .= ("received" :: String)
           , "time"  .= t
           , "name"  .= name
           , "line"  .= line ]
  toJSON (Error name description t) =
    object [ "event"       .= ("error" :: String)
           , "time"        .= t
           , "name"        .= name
           , "description" .= description ]

run :: IO ()
run = do
  fleet   <- newTVarIO Map.empty
  channel <- newTChanIO

  void $ runEffectsConcurrently
    (writeChannelToStdout channel)
    (forEveryStdinLine $ handle fleet channel)

handle :: Fleet -> TChan Event -> Maybe Command -> IO ()
handle fleet channel (Just (Connect name host port)) =
  void . async $ catchError doConnect (writeException name channel)
  where
    doConnect =
      connect host port $ \(socket, _) ->
        onConnect fleet channel socket name host port
handle fleet channel (Just (Send name line)) =
  do envoy <- Map.lookup name <$> atomically (readTVar fleet)
     case envoy of
       Just (Envoy f) -> f line
       Nothing        -> writeError name channel "No server by that name"

onConnect :: Fleet -> TChan Event -> Socket -> String
          -> String -> String -> IO ()
onConnect fleet channel socket name host port = do
  writeTimestamped channel $ Connected name host port

  (output, input) <- spawn Unbounded
  atomically . addEnvoy fleet name $ output

  flip catchError (writeException name channel) . void $ runEffectsConcurrently
    (receiveLines name socket channel)
    (inputToSocket socket $ input)

receiveLines :: String -> Socket -> TChan Event -> Effect IO ()
receiveLines name socket channel = do
  for (socketLines socket) $ writeTimestamped channel . Received name
  writeError name channel "Connection closed"

writeException :: (Functor m, MonadIO m) => String -> TChan Event
               -> IOException -> m ()
writeException name channel = writeError name channel . show

writeError :: (Functor m, MonadIO m) => String -> TChan Event -> String -> m ()
writeError name channel = writeTimestamped channel . Error name

timestamped :: (Functor m, MonadIO m) => (Timestamp -> a) -> m a
timestamped f = fmap f (liftIO getPOSIXMsecs)

writeTimestamped :: (Functor m, MonadIO m) => TChan a -> (Timestamp -> a)
                 -> m ()
writeTimestamped c f = timestamped f >>= liftIO . atomically . writeTChan c

addEnvoy :: Fleet -> String -> Output String -> STM ()
addEnvoy m name output = modifyTVar m . Map.insert name . Envoy $ f
  where f x = do True <- atomically $ send output (x ++ "\n")
                 return ()
