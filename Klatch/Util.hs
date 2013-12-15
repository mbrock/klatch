{-# LANGUAGE OverloadedStrings #-}

module Klatch.Util where

import Control.Monad
import Control.Monad.STM

import Control.Concurrent.Async
import Control.Concurrent.STM.TChan

import Data.Aeson

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SB
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import Network.Simple.TCP (Socket)

import Data.Time.Clock.POSIX

import Pipes
import Pipes.Concurrent (Input, fromInput)
import Pipes.Network.TCP (fromSocket, toSocket)

import qualified Pipes.Prelude as P
import qualified Pipes.Parse as PP
import qualified Pipes.ByteString as PBS

runEffectsConcurrently :: Effect IO a -> Effect IO b -> IO ()
runEffectsConcurrently a b = void $ concurrently (runEffect a) (runEffect b)

encoder :: ToJSON a => Pipe a String IO ()
encoder = P.map (fromUTF8 . encode)

decoder :: FromJSON a => Pipe String (Maybe a) IO ()
decoder = P.map (decode . toUTF8)

contents :: TChan a -> Producer a IO ()
contents c = forever $ liftIO (atomically $ readTChan c) >>= yield

writeToSocket :: Socket -> Input String -> IO ()
writeToSocket socket input = runEffect $
  fromInput input >-> P.map toStrictUTF8 >-> toSocket socket

socketLines :: Socket -> Producer String IO ()
socketLines s = p >-> P.filter (/= "") >-> P.map fromStrictUTF8
  where p = PP.concat . PBS.lines $ fromSocket s 4096

toUTF8 :: String -> BS.ByteString
toUTF8 = E.encodeUtf8 . T.pack

fromUTF8 :: BS.ByteString -> String
fromUTF8 = T.unpack . E.decodeUtf8

fromStrictUTF8 :: SB.ByteString -> String
fromStrictUTF8 = fromUTF8 . BS.fromChunks . (:[])

toStrictUTF8 :: String -> SB.ByteString
toStrictUTF8 = SB.concat . BS.toChunks . toUTF8

getPOSIXMsecs :: IO Int
getPOSIXMsecs = fmap ((`div` 1000000000) . fromEnum) getPOSIXTime

printIndentedList :: Int -> [String] -> IO ()
printIndentedList n xs = forM_ xs (putStrLn . (replicate n ' ' ++))

newline :: IO ()
newline = putStr "\n"

logWrite :: Pipe Event Event IO ()
logWrite = logWithPrefix "> "

logRead :: Pipe (Maybe Command) (Maybe Command) IO ()
logRead = logWithPrefix "< "

logWithPrefix :: Show a => String -> Pipe a a IO ()
logWithPrefix p = P.tee $ P.map ((p ++) . show) >-> P.stdoutLn
