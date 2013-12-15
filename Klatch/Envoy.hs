{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Klatch.Envoy where

import Klatch.Util

import Control.Applicative          ((<$>), (<*>))
import Control.Arrow                ((***))
import Control.Concurrent.Async     (concurrently, async)
import Control.Concurrent.STM       (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan)
import Control.Concurrent.STM.TVar  (TVar, newTVarIO, readTVar, modifyTVar)
import Control.Exception            (IOException)
import Control.Monad                (void)
import Control.Monad.Error          (catchError)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Network.Simple.TCP           (Socket, connect)

import Pipes              (for, runEffect)
import Pipes.Concurrent   (Output, Input, Buffer (Unbounded), spawn, send)

import Data.Aeson   (FromJSON, ToJSON, Value (..), (.:), (.=),
                     object, parseJSON, toJSON)
import GHC.Generics (Generic)

import           Data.Map   (Map)
import qualified Data.Map as Map

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

  runEffectsConcurrently
    (writeChannelToStdout channel)
    (forEveryStdinLine $ handle fleet channel)

handle :: Fleet -> TChan Event -> Maybe Command -> IO ()
handle f c (Just cmd) =
  case cmd of
    Connect name host port -> handleConnect f c name host port
    Send name line         -> handleSend f c name line
    Unknown (Just s)       -> writeError "" c ("Unknown command " ++ s)
    Unknown Nothing        -> writeError "" c "Unreadable command"
handle _ c Nothing = writeError "" c "Parse error"

handleConnect :: Fleet -> TChan Event -> String -> String -> String -> IO ()
handleConnect fleet channel name host port =
  void . async . flip catchError (writeException name channel) $
    connect host port $ \(socket, _) ->
      onConnect fleet channel socket name host port

handleSend :: Fleet -> TChan Event -> String -> String -> IO ()
handleSend fleet channel name line =
  Map.lookup name <$> atomically (readTVar fleet) >>=
    maybe (writeError name channel "No such server") (flip sendTo line)

onConnect :: Fleet -> TChan Event -> Socket -> String
          -> String -> String -> IO ()
onConnect fleet channel socket name host port = do
  writeTimestamped channel $ Connected name host port
  flip catchError (writeException name channel) . void $ concurrently
    (receiveLines name socket channel)
    (addEnvoy fleet name `outputtingTo` writeToSocket socket)

outputtingTo :: (Output a -> IO ()) -> (Input a -> IO ()) -> IO ()
outputtingTo f g = spawn Unbounded >>= uncurry (>>) . (f *** g)

receiveLines :: String -> Socket -> TChan Event -> IO ()
receiveLines name socket channel = runEffect $ do
  for (socketLines socket) $ writeTimestamped channel . Received name
  writeError name channel "Connection closed"

addEnvoy :: Fleet -> String -> Output String -> IO ()
addEnvoy m name o = atomically . modifyTVar m . Map.insert name . Envoy $ f
  where f x = do True <- atomically $ send o (x ++ "\n")
                 return ()

writeException :: (Functor m, MonadIO m) => String -> TChan Event
               -> IOException -> m ()
writeException name channel = writeError name channel . show

writeError :: (Functor m, MonadIO m) => String -> TChan Event -> String -> m ()
writeError name channel = writeTimestamped channel . Error name

timestamped :: (Functor m, MonadIO m) => (Timestamp -> a) -> m a
timestamped = (<$> liftIO getPOSIXMsecs)

writeTimestamped :: (Functor m, MonadIO m) => TChan a -> (Timestamp -> a)
                 -> m ()
writeTimestamped c f = timestamped f >>= liftIO . atomically . writeTChan c
