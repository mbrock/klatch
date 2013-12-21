{-# LANGUAGE OverloadedStrings #-}

module Klatch.Common.Util where

import Klatch.Common.Types

import Control.Applicative ((<$>))
import Control.Arrow       ((***))
import Control.Concurrent  (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM
import Data.Attoparsec.ByteString (IResult (..))
import Data.Aeson
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Language.Haskell.HsColour.ANSI
import Network.IRC.ByteString.Parser
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

silentDecoder :: FromJSON a => Pipe T.Text a IO ()
silentDecoder = decoder >-> skipNothings

contents :: TChan a -> Producer a IO ()
contents c = forever $ liftIO (atomically $ readTChan c) >>= yield

toChannel :: TChan a -> Consumer a IO ()
toChannel c = for cat (liftIO . atomically . writeTChan c)

ignore :: Monad m => Consumer a m ()
ignore = for cat (const $ return ())

into :: Monad m => (a -> m ()) -> Pipe a a m ()
into f = for cat (\x -> lift (f x) >> yield x)

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

fromLazyUTF8 :: BSL.ByteString -> T.Text
fromLazyUTF8 = fromUTF8 . BSL.toStrict

getPOSIXMsecs :: IO Int
getPOSIXMsecs = fmap (truncate . (* 1000) . toRational) getPOSIXTime

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

formatTimestamp :: Timestamp -> String
formatTimestamp x = formatTime defaultTimeLocale "%c"
  (posixSecondsToUTCTime (fromIntegral (x `div` 1000) :: NominalDiffTime))

writeLog :: MonadIO m => String -> m ()
writeLog x = liftIO $ formatLogLine x >>= putStrLn

string :: T.Text -> String
string = T.unpack

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

plural :: Int -> String -> String
plural 0 s = "no " ++ s ++ "s"
plural 1 s = "one " ++ s
plural x s | x <= length countingWords = countingWords !! x ++ s ++ "s"
plural x s = show x ++ s ++ "s"

countingWords :: [String]
countingWords = ["", "", "two", "three", "four", "five", "six", "seven"]

continue :: Monad m => Pipe a a m ()
continue = for cat yield

awaitOnce :: Monad m => (a -> Maybe (Pipe a a m ())) -> Pipe a a m ()
awaitOnce f = forever $ await >>= \x -> case f x of
                                          Just m  -> m >> continue
                                          Nothing -> yield x

parseIRCLine :: T.Text -> Maybe IRCMsg
parseIRCLine line =
  case toIRCMsg . toUTF8 $ T.append line "\r\n" of
    Done _ r -> Just r
    _ -> Nothing

toIRCLine :: IRCMsg -> T.Text
toIRCLine = fromUTF8 . fromIRCMsg
