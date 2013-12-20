{-# LANGUAGE NamedFieldPuns #-}

module Klatch.Envoy.Queue where

import Control.Applicative    ((<$>))
import Control.Concurrent.STM (STM, TChan, atomically, writeTChan)
import Control.Exception      (IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text, pack)
import Pipes                  (Producer, Consumer, for, cat)
import Prelude         hiding (sequence)

import Klatch.Envoy.Types
import Klatch.Util (contents, getPOSIXMsecs)

data Queue = Queue { input  :: TChan Text
                   , output :: Text -> STM () }

readFrom :: Queue -> Producer Text IO ()
readFrom Queue { input } = contents input

writeTo :: Queue -> Consumer Text IO ()
writeTo Queue { output } = for cat (liftIO . atomically . output)

writeException :: (Functor m, MonadIO m)
               => Text -> TChan (EventWithMetadata a i) -> i
                       -> IOException -> m ()
writeException name channel i = writeError name channel i . pack . show

writeError :: (Functor m, MonadIO m)
           => Text -> TChan (EventWithMetadata a i) -> i -> Text -> m ()
writeError name channel i = writeEvent channel i . Error name

timestamped :: (Functor m, MonadIO m) => (Timestamp -> a) -> m a
timestamped = (<$> liftIO getPOSIXMsecs)

writeEvent :: (Functor m, MonadIO m)
           => TChan (EventWithMetadata a i) -> i -> (Event a) -> m ()
writeEvent c i e = do
  timestamp <- liftIO getPOSIXMsecs
  liftIO . atomically . writeTChan c $
    EventWithMetadata { payload   = e
                      , timestamp = timestamp
                      , version   = envoyVersion
                      , sequence  = i }
