module Klatch.Embassy.FileLog where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Data.Map ((!))
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (doesFileExist)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map

import Klatch.Params
import Klatch.Util (writeLog, bolded)

newtype (Show a, Read a) => FileLog a = FileLog { writeToLog :: a -> IO () }

defaults :: ParamSpec
defaults = Map.fromList [ ("EMBASSY_LOG", Nothing) ]

startFileLog :: (Read a, Show a) => (FileLog a -> [a] -> IO ()) -> IO ()
startFileLog f = do
  params <- getParameters "embassy" defaults
  uncurry f =<< makeLog (params ! "EMBASSY_LOG")

makeLog :: (Read a, Show a) => String -> IO (FileLog a, [a])
makeLog path = do
  old <- createOrRead path []  
  xs  <- newTVarIO old
  
  let fileLog = FileLog $ \x -> 
        addToLog xs x >>= BS.writeFile path . encodeUtf8 . pack . show
               
  return (fileLog, old)

createOrRead :: (Read a, Show a) => String -> a -> IO a
createOrRead path x = do
  exists <- doesFileExist path
  case exists of
    True -> read . unpack . decodeUtf8 <$> BS.readFile path
    False -> do
      writeLog $ "Creating embassy log at " ++ bolded path ++ "."
      writeFile path (show x)
      return x

addToLog :: Show a => TVar [a] -> a -> IO [a]
addToLog v x = atomically $ do
  xs <- readTVar v
  writeTVar v (x:xs)
  return (x:xs)
  