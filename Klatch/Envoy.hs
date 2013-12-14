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

import Network.Simple.TCP

import Pipes
import Pipes.Concurrent
import qualified Pipes.Parse as PP
import qualified Pipes.ByteString as PBS
import qualified Pipes.Prelude as P
import Pipes.Network.TCP (fromSocket, toSocket)

run :: IO ()
run = do
  fleet <- newTVarIO Map.empty
  channel <- newTChanIO :: IO (TChan Event)
  void . async . runEffect $
    for (contents channel) (yield . fromUTF8 . encode) >-> P.stdoutLn
  runEffect $
    for P.stdinLn (handle fleet channel . decode . toUTF8)
  
toUTF8 :: String -> BS.ByteString
toUTF8 = E.encodeUtf8 . T.pack

fromUTF8 :: BS.ByteString -> String
fromUTF8 = T.unpack . E.decodeUtf8

fromStrictUTF8 :: SB.ByteString -> String
fromStrictUTF8 = fromUTF8 . BS.fromChunks . (:[])

toStrictUTF8 :: String -> SB.ByteString
toStrictUTF8 = SB.concat . BS.toChunks . toUTF8

contents :: TChan a -> Producer' a IO ()
contents c = forever $ liftIO (atomically $ readTChan c) >>= yield
  
type Timestamp = Int

data Command = Connect String String String
             | Send String String
             | Unknown (Maybe String)
               deriving (Eq, Show, Generic)
                      
data Event = Connected String String String Timestamp
           | Received String String Timestamp
             deriving (Eq, Show, Generic)

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
    object ["event" .= ("connected" :: String),
            "time" .= t,
            "name" .= name,
            "host" .= host,
            "port" .= port]
  toJSON (Received name line t) =
    object ["event" .= ("received" :: String),
            "time" .= t,
            "name" .= name,
            "line" .= line]
    
timestamped :: (Timestamp -> a) -> IO a
timestamped f = fmap (f . (`div` 1000000000) . fromEnum) getPOSIXTime
                      
handle :: Fleet -> TChan Event -> Maybe Command -> Effect IO ()
handle fleet channel (Just (Connect name host port)) =
  void . liftIO . async $ connect host port $ \(socket, _) -> do
    event <- liftIO $ timestamped $ Connected name host port
    atomically . writeTChan channel $ event
    
    async . runEffect $ 
      for (PP.concat . PBS.lines $ fromSocket socket 4096) $ \line ->
        if line /= ""
        then do
          event <- liftIO . timestamped . Received name . fromStrictUTF8 $ line
          liftIO . atomically $ writeTChan channel event
        else return ()
      
    (output, input) <- spawn Unbounded
    liftIO . atomically $ addEnvoy fleet name output
    
    runEffect $ fromInput input >-> P.map toStrictUTF8 >-> toSocket socket
    
type Fleet = TVar (Map String (Output String))

addEnvoy :: Fleet -> String -> Output String -> STM ()
addEnvoy f name output =
  modifyTVar f (Map.insert name output)
