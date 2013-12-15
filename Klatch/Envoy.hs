{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Klatch.Util
import qualified Klatch.Busser as Busser

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

import Pipes              (Consumer, cat, for, runEffect, (>->))
import Pipes.Concurrent   (Output, Input, Buffer (Unbounded), spawn, send)

import Data.Aeson   (FromJSON, ToJSON, Value (..), (.:), (.=),
                     object, parseJSON, toJSON)
import GHC.Generics (Generic)

import           Data.Map   (Map)
import qualified Data.Map as Map

envoyVersion :: Int
envoyVersion = 1

type Timestamp = Int

data Command = Connect String String String
             | Send String String
             | Unknown (Maybe String)
               deriving (Eq, Show, Generic)

type EventMetadata = (Timestamp, Int)

data Event = Connected String String String EventMetadata
           | Received String String EventMetadata
           | Error String String EventMetadata
           | Started EventMetadata
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
  toJSON (Error name description (t, v)) =
    object [ "event"                .= ("error" :: String)
           , "time"                 .= t
           , "version"              .= v
           , "name"                 .= name
           , "description"          .= description ]

main :: IO ()
main = do
  (busser, _) <- Busser.getParameters >>= Busser.connect
  fleet       <- newTVarIO Map.empty
  channel     <- newTChanIO
  
  writeEvent channel Started

  runEffectsConcurrently
    (contents channel >-> logWrite >-> encoder >-> Busser.writeTo busser)
    (Busser.readFrom busser >-> decoder >-> logRead >-> handler fleet channel)

handler :: Fleet -> TChan Event -> Consumer (Maybe Command) IO ()
handler f c = for cat (liftIO . handle f c)
    
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
  writeEvent channel $ Connected name host port
  flip catchError (writeException name channel) . void $ concurrently
    (receiveLines name socket channel)
    (addEnvoy fleet name `outputtingTo` writeToSocket socket)

outputtingTo :: (Output a -> IO ()) -> (Input a -> IO ()) -> IO ()
outputtingTo f g = spawn Unbounded >>= uncurry (>>) . (f *** g)

receiveLines :: String -> Socket -> TChan Event -> IO ()
receiveLines name socket channel = runEffect $ do
  for (socketLines socket) $ writeEvent channel . Received name
  writeError name channel "Connection closed"

addEnvoy :: Fleet -> String -> Output String -> IO ()
addEnvoy m name o = atomically . modifyTVar m . Map.insert name . Envoy $ f
  where f x = do True <- atomically $ send o (x ++ "\n")
                 return ()

writeException :: (Functor m, MonadIO m) => String -> TChan Event
               -> IOException -> m ()
writeException name channel = writeError name channel . show

writeError :: (Functor m, MonadIO m) => String -> TChan Event -> String -> m ()
writeError name channel = writeEvent channel . Error name

timestamped :: (Functor m, MonadIO m) => (Timestamp -> a) -> m a
timestamped = (<$> liftIO getPOSIXMsecs)

writeEvent :: (Functor m, MonadIO m) => TChan a -> (EventMetadata -> a) -> m ()
writeEvent c f = do 
  timestamp <- liftIO getPOSIXMsecs
  liftIO . atomically . writeTChan c $ f (timestamp, envoyVersion)
