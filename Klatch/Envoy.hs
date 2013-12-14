{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Klatch.Envoy where

import Klatch.Util

import Control.Applicative
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Data.Aeson
import GHC.Generics (Generic)

import           Data.Map   (Map)
import qualified Data.Map as Map

import Pipes
import Pipes.Concurrent

import Data.Time.Clock.POSIX
import Network.Simple.TCP (Socket, connect)

type Timestamp = Int

data Command = Connect String String String
             | Send String String
             | Unknown (Maybe String)
               deriving (Eq, Show, Generic)

data Event = Connected String String String Timestamp
           | Received String String Timestamp
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
           , "time" .= t
           , "name" .= name
           , "host" .= host
           , "port" .= port ]
  toJSON (Received name line t) =
    object [ "event" .= ("received" :: String)
           , "time" .= t
           , "name" .= name
           , "line" .= line ]

run :: IO ()
run = do
  fleet   <- newTVarIO Map.empty
  channel <- newTChanIO

  void $ runEffectsConcurrently
    (writeChannelToStdout channel)
    (forEveryStdinLine $ handle fleet channel)

handle :: Fleet -> TChan Event -> Maybe Command -> IO ()
handle fleet channel (Just (Connect name host port)) =
    void . async $ connect host port $ \(socket, _) ->
        onConnect fleet channel socket name host port

onConnect :: Fleet -> TChan Event -> Socket -> String
          -> String -> String -> IO ()
onConnect fleet channel socket name host port = do
  writeTimestamped channel $ Connected name host port

  (output, input) <- spawn Unbounded
  atomically . addEnvoy fleet name $ output

  void $ runEffectsConcurrently
    (for (socketLines socket) $ writeTimestamped channel . Received name)
    (inputToSocket socket $ input)

timestamped :: (Functor m, MonadIO m) => (Timestamp -> a) -> m a
timestamped f =
    fmap (f . (`div` 1000000000) . fromEnum) (liftIO getPOSIXTime)

writeTimestamped :: (Functor m, MonadIO m) =>
                    TChan a -> (Timestamp -> a) -> m ()
writeTimestamped c f =
    timestamped f >>= liftIO . atomically . writeTChan c

addEnvoy :: Fleet -> String -> Output String -> STM ()
addEnvoy m name output = modifyTVar m . Map.insert name . Envoy $ f
    where f x = do True <- atomically (send output x)
                   return ()
