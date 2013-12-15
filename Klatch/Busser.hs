{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad (void)

import System.Posix.Env (getEnv)

import Data.Map (Map)
import qualified Data.Map as Map

import Options.Applicative.Utils (tabulate)

defaults :: Map String (Maybe String)
defaults =
  Map.fromList [ ("BUSSER_AMQP_HOST"        , Just "127.0.0.1")
               , ("BUSSER_AMQP_VHOST"       , Just "/")
               , ("BUSSER_AMQP_USER"        , Just "guest")
               , ("BUSSER_AMQP_PASSWORD"    , Just "guest")
               , ("BUSSER_AMQP_QUEUE"       , Nothing)
               , ("BUSSER_AMQP_EXCHANGE"    , Nothing)
               , ("BUSSER_AMQP_BINDING_KEY" , Just "busser") ]
  
onlyJusts :: Ord k => Map k (Maybe v) -> Map k v
onlyJusts = Map.mapMaybe id

parameters :: [String]
parameters = Map.keys defaults
  
getParametersAndUsedDefaults :: IO (Map String String, [String])
getParametersAndUsedDefaults = do
  env <- fmap Map.fromList (mapM (\k -> fmap (k,) (getEnv k)) parameters)
  let params = Map.union (onlyJusts env) (onlyJusts defaults)
  return (params, Map.keys $ Map.difference params (onlyJusts env))

getParameters :: IO (Map String String)
getParameters = do
  (params, usedDefaults) <- getParametersAndUsedDefaults
  putStrLn "Using the following parameters:"
  let f (k, v) = (k, v ++ if elem k usedDefaults
                          then " (default)"
                          else "")
  putStrLn . unlines . tabulate . map f $ Map.assocs params
  return params

main :: IO ()
main = do
  putStrLn "\nStarting Busser...\n"
  void getParameters
