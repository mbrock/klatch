{-# LANGUAGE NamedFieldPuns #-}

module Klatch.Envoy.Queue where

import Control.Applicative ((<$>))
import Control.Concurrent.STM (STM, TChan, atomically, writeTChan)
import Control.Exception (IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Pipes (Producer, Consumer, for, cat)

import Klatch.Envoy.Types
import Klatch.Util (contents, getPOSIXMsecs)

data Queue = Queue { input  :: TChan String
                   , output :: String -> STM () }

readFrom :: Queue -> Producer String IO ()
readFrom Queue { input } = contents input

writeTo :: Queue -> Consumer String IO ()
writeTo Queue { output } = for cat (liftIO . atomically . output)

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
