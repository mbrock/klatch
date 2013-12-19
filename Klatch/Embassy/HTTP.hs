{-# LANGUAGE OverloadedStrings #-}

module Klatch.Embassy.HTTP (run) where

import Klatch.Envoy.Types
import Klatch.Envoy.JSON ()

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Control.Applicative                 ((<$>))
import Control.Concurrent.STM              (atomically)
import Control.Concurrent.STM.TChan        (TChan, readTChan)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Data.Aeson                          (encode)
import Data.ByteString.Lazy                (ByteString)
import Network.HTTP.Types                  (status200)
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Middleware.Gzip         (gzip, def)

import qualified Network.Wai.Handler.Warp as Warp

run :: MonadIO m => TChan ParsedEvent -> m ()
run events = liftIO . Warp.run 3000 . gzip def $
  eventSourceAppIO (makeServerEvent . encode <$> atomically (readTChan events))

makeServerEvent :: ByteString -> ServerEvent
makeServerEvent bs = ServerEvent Nothing Nothing [fromLazyByteString bs]
