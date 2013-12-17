{-# LANGUAGE OverloadedStrings #-}

module Klatch.Util where

import Control.Applicative ((<$>))
import Control.Arrow       ((***))
import Control.Concurrent  (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Data.Aeson
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Language.Haskell.HsColour.ANSI
import Network.Simple.TCP  (Socket)
import Pipes
import Pipes.Concurrent    (Output, Input, Buffer (Unbounded), spawn, fromInput)
import Pipes.Network.TCP   (fromSocket, toSocket)
import System.Locale

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as E
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as EL
import qualified Pipes.ByteString        as PBS
import qualified Pipes.Parse             as PP
import qualified Pipes.Prelude           as P
import qualified System.Posix.Signals    as Sig

outputtingTo :: (Output a -> IO ()) -> (Input a -> IO ()) -> IO ()
outputtingTo f g = spawn Unbounded >>= uncurry (>>) . (f *** g)

runEffectsConcurrently :: Effect IO a -> Effect IO b -> IO ()
runEffectsConcurrently a b = void $ concurrently (runEffect a) (runEffect b)

encoder :: ToJSON a => Pipe a T.Text IO ()
encoder = P.map (TL.toStrict . EL.decodeUtf8 . encode)

decoder :: FromJSON a => Pipe T.Text (Maybe a) IO ()
decoder = P.map (decode . BSL.fromStrict . E.encodeUtf8)

contents :: TChan a -> Producer a IO ()
contents c = forever $ liftIO (atomically $ readTChan c) >>= yield

ignore :: Monad m => Consumer a m ()
ignore = for cat (const $ return ())

into :: Monad m => (a -> m ()) -> Consumer a m ()
into f = for cat (lift . f)

writeToSocket :: Socket -> Input T.Text -> IO ()
writeToSocket socket input = runEffect $
  fromInput input >-> P.map toUTF8 >-> toSocket socket

socketLines :: Socket -> Producer T.Text IO ()
socketLines s = p >-> P.filter (/= "") >-> P.map fromUTF8
  where p = PP.concat . PBS.lines $ fromSocket s 4096

toUTF8 :: T.Text -> BS.ByteString
toUTF8 = E.encodeUtf8

fromUTF8 :: BS.ByteString -> T.Text
fromUTF8 = E.decodeUtf8

getPOSIXMsecs :: IO Int
getPOSIXMsecs = fmap ((`div` 1000000000) . fromEnum) getPOSIXTime

printIndentedList :: Int -> [String] -> IO ()
printIndentedList n xs = forM_ xs (putStrLn . (replicate n ' ' ++))

newline :: IO ()
newline = putStr "\n"

loggingWrites :: Show a => Pipe a a IO ()
loggingWrites = loggingWithPrefix ">> "

loggingReads :: Show a => Pipe a a IO ()
loggingReads = loggingWithPrefix "<< "

loggingWithPrefix :: Show a => String -> Pipe a a IO ()
loggingWithPrefix p = P.tee $ P.mapM (showWithPrefix p) >-> P.stdoutLn

showWithPrefix :: (Functor m, MonadIO m, Show a) => String -> a -> m String
showWithPrefix p x = formatLogLine (bolded p ++ show x)

formatLogLine :: (Functor m, MonadIO m) => String -> m String
formatLogLine x =
  do t <- formatTime defaultTimeLocale "%c" <$> liftIO getCurrentTime
     return (dimmed t ++ "\n  " ++ x ++ "\n")

writeLog :: String -> IO ()
writeLog x = formatLogLine x >>= putStrLn

dimmed :: String -> String
dimmed = highlight [Dim]

bolded :: String -> String
bolded = highlight [Bold]

onCtrlC :: IO () -> IO ()
onCtrlC m =
  void $ Sig.installHandler Sig.keyboardSignal (Sig.CatchOnce m) Nothing

sleep :: Int -> IO ()
sleep = threadDelay . (* 1000000)

onlyJusts :: Ord k => Map.Map k (Maybe v) -> Map.Map k v
onlyJusts = Map.mapMaybe id

skipNothings :: Monad m => Pipe (Maybe a) a m ()
skipNothings = forever $ do
  x <- await
  case x of
    Nothing -> return ()
    Just y -> yield y
