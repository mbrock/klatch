module Klatch.Embassy.FileLog where

import Control.Applicative     ((<$>))
import Control.Concurrent.STM
import Data.Aeson              (FromJSON, ToJSON, encode, decode)
import Data.Map                ((!))
import Data.Maybe              (fromJust)
import System.Directory        (doesFileExist)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import qualified Data.Map             as Map

import Klatch.Common.Params
import Klatch.Common.Util (writeLog, bolded)

newtype (FromJSON a, ToJSON a) => FileLog a =
  FileLog { writeToLog :: a -> IO () }

defaults :: ParamSpec
defaults = Map.fromList [ ("EMBASSY_LOG", Nothing) ]

startFileLog :: (FromJSON a, ToJSON a) => (FileLog a -> [a] -> IO ()) -> IO ()
startFileLog f = do
  params <- getParameters "embassy" defaults
  uncurry f =<< (makeLog . T.unpack $ params ! "EMBASSY_LOG")

makeLog :: (FromJSON a, ToJSON a) => String -> IO (FileLog a, [a])
makeLog path = do
  old <- createOrRead path []
  xs  <- newTVarIO old

  let fileLog = FileLog $ \x -> addToLog xs x >>= BS.writeFile path . encode

  return (fileLog, reverse old)

createOrRead :: (FromJSON a, ToJSON a) => String -> a -> IO a
createOrRead path x = do
  exists <- doesFileExist path
  case exists of
    True -> fromJust . decode <$> BS.readFile path
    False -> do
      writeLog $ "Creating embassy log at " ++ bolded path ++ "."
      BS.writeFile path (encode x)
      return x

addToLog :: ToJSON a => TVar [a] -> a -> IO [a]
addToLog v x = atomically $ do
  xs <- readTVar v
  writeTVar v (x:xs)
  return (x:xs)
