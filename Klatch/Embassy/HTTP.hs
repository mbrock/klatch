{-# LANGUAGE OverloadedStrings #-}

module Klatch.Embassy.HTTP (run) where

import Klatch.Envoy.Types
import Klatch.Util

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Control.Concurrent.STM              (atomically)
import Control.Concurrent.STM.TChan        (TChan, cloneTChan, readTChan)
import Control.Monad                       (forever)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Data.Aeson                          (encode)
import Data.ByteString.Lazy                (ByteString)
import Network.Wai.EventSource
import Network.Wai.Middleware.Gzip         (gzip, def)

import qualified Data.Conduit as C
import qualified Network.Wai.Handler.Warp as Warp

port :: Int
port = 3000

url :: String
url = "http://localhost:" ++ show port ++ "/"

run :: MonadIO m => TChan ParsedEvent -> m ()
run c = liftIO $ do
  writeLog $ "Starting web server on " ++ bolded url
  Warp.run port . gzip def . eventSourceAppSource $ stream c

stream :: TChan ParsedEvent -> C.Source IO ServerEvent
stream c = do
  c' <- liftIO . atomically $ cloneTChan c
  liftIO $ writeLog "Accepting web visitor."
  forever $ do
    x <- liftIO . atomically $ readTChan c'
    C.yield $ makeServerEvent (encode x)

makeServerEvent :: ByteString -> ServerEvent
makeServerEvent bs = ServerEvent Nothing Nothing [fromLazyByteString bs]
