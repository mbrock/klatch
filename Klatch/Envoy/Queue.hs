{-# LANGUAGE NamedFieldPuns #-}

module Klatch.Envoy.Queue where

import Control.Applicative    ((<$>))
import Control.Concurrent.STM (STM, TChan, atomically, writeTChan)
import Control.Exception      (IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text, pack)
import Pipes                  (Producer, Consumer, for, cat)
import Prelude         hiding (sequence)

import Klatch.Common.Types
import Klatch.Common.Util (contents, getPOSIXMsecs)

data Queue = Queue { input  :: TChan Text
                   , output :: Text -> STM () }

readFrom :: Queue -> Producer Text IO ()
readFrom Queue { input } = contents input

writeTo :: Queue -> Consumer Text IO ()
writeTo Queue { output } = for cat (liftIO . atomically . output)

writeException :: (Functor m, MonadIO m)
               => Text -> TChan Event -> Sequence -> IOException -> m ()
writeException name channel i = writeError name channel i . pack . show

writeError :: (Functor m, MonadIO m)
           => Text -> TChan Event -> Sequence -> Text -> m ()
writeError name channel i = writeEvent channel i . SocketError name

timestamped :: (Functor m, MonadIO m) => (Timestamp -> a) -> m a
timestamped = (<$> liftIO getPOSIXMsecs)

writeEvent :: (Functor m, MonadIO m)
           => TChan Event -> Sequence -> Payload -> m ()
writeEvent c i p = do
  t <- liftIO getPOSIXMsecs
  liftIO . atomically . writeTChan c $ Event t i p
