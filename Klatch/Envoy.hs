{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Klatch.Envoy where

import Control.Applicative
import Control.Monad
import Control.Monad.STM

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Data.Aeson
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SB

import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as E

import           Data.Map   (Map)
import qualified Data.Map as Map

import Data.Time.Clock.POSIX

import Network.Simple.TCP (Socket, connect)

import Pipes
import Pipes.Concurrent
import qualified Pipes.Parse as PP
import qualified Pipes.ByteString as PBS
import qualified Pipes.Prelude as P
import Pipes.Network.TCP (fromSocket, toSocket)

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

run :: IO ()
run = do
  fleet <- newTVarIO Map.empty
  channel <- newTChanIO :: IO (TChan Event)
  void $ concurrently
    (runEffect $ contents channel >-> P.map (fromUTF8 . encode) >-> P.stdoutLn)
    (runEffect $ for P.stdinLn (handle fleet channel . decode . toUTF8))

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

handle :: Fleet -> TChan Event -> Maybe Command -> Effect IO ()
handle fleet channel (Just (Connect name host port)) =
    liftIO . void . async $ connect host port $ \(socket, _) ->
        onConnect fleet channel socket name host port

onConnect :: Fleet -> TChan Event -> Socket -> String
          -> String -> String -> IO ()
onConnect fleet channel socket name host port = do
  writeTimestamped channel $ Connected name host port

  async . runEffect . for (socketLines socket) $
    writeTimestamped channel . Received name

  (output, input) <- spawn Unbounded
  atomically . addEnvoy fleet name $ output
  runEffect . inputToSocket socket $ input

inputToSocket :: Socket -> Input String -> Effect IO ()
inputToSocket socket input =
    fromInput input >-> P.map toStrictUTF8 >-> toSocket socket

socketLines :: Socket -> Producer String IO ()
socketLines s = p >-> P.filter (/= "") >-> P.map fromStrictUTF8
  where p = PP.concat . PBS.lines $ fromSocket s 4096

contents :: TChan a -> Producer a IO ()
contents c = forever $ liftIO (atomically $ readTChan c) >>= yield

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

toUTF8 :: String -> BS.ByteString
toUTF8 = E.encodeUtf8 . T.pack

fromUTF8 :: BS.ByteString -> String
fromUTF8 = T.unpack . E.decodeUtf8

fromStrictUTF8 :: SB.ByteString -> String
fromStrictUTF8 = fromUTF8 . BS.fromChunks . (:[])

toStrictUTF8 :: String -> SB.ByteString
toStrictUTF8 = SB.concat . BS.toChunks . toUTF8
