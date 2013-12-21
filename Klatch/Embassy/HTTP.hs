{-# LANGUAGE OverloadedStrings #-}

module Klatch.Embassy.HTTP (run) where

import Klatch.Common.Types
import Klatch.Common.Util

import Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import Control.Concurrent                  (threadDelay)
import Control.Concurrent.STM              (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad                       (forever)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Data.Aeson                          (encode, decode)
import Data.ByteString.Lazy                (ByteString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Middleware.Gzip         (gzip, def)
import Network.Wai.Middleware.Static       (staticPolicy, noDots)

import qualified Data.Conduit as C
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy as LBS

port :: Int
port = 3000

url :: String
url = "http://localhost:" ++ show port ++ "/"

run :: MonadIO m => TChan ParsedEvent -> TChan Command -> m ()
run c q = liftIO $ do
  writeLog $ "Starting web server on " ++ bolded url
  Warp.run port
    . gzip def
    . staticPolicy noDots
    $ getOrPost (eventSourceAppSource $ stream c)
                (handleClientEvents q)

handleClientEvents :: TChan Command -> Application
handleClientEvents q req = do
  x <- lazyRequestBody req
  case decode x of
    Just cmd -> do
      atomically (writeTChan q cmd)
      return (responseLBS ok200 [] LBS.empty)
    Nothing ->
      return (responseLBS imATeaPot418 [] LBS.empty)

getOrPost :: Application -> Application -> Application
getOrPost g _ req | requestMethod req == methodGet  = g req
getOrPost _ p req | requestMethod req == methodPost = p req
getOrPost g _ req                                   = g req

stream :: TChan ParsedEvent -> C.Source IO ServerEvent
stream c = do
  c' <- liftIO . atomically $ cloneTChan c
  liftIO $ writeLog "Accepting web visitor."
  forever $ do
    x <- liftIO . atomically $ readTChan c'
    liftIO $ threadDelay (truncate $ 1000000.0/400.0)
    C.yield $ makeServerEvent (encode x)

makeServerEvent :: ByteString -> ServerEvent
makeServerEvent bs = ServerEvent Nothing Nothing [fromLazyByteString bs]
