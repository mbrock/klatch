{-# LANGUAGE TupleSections #-}

module Klatch.Params where

import Klatch.Util

import Control.Monad             (when)
import Data.Map                  (Map)
import Data.List                 ((\\))
import Options.Applicative.Utils (tabulate)
import System.Exit               (exitFailure)
import System.Posix.Env          (getEnv)
import qualified Data.Map as Map

type ParamSpec = Map String (Maybe String)
type Params = Map String String

names :: ParamSpec -> [String]
names = Map.keys

getParametersAndUsedDefaults :: ParamSpec -> IO (Params, [String])
getParametersAndUsedDefaults spec = do
  env <- fmap Map.fromList (mapM (\k -> fmap (k,) (getEnv k)) (names spec))
  let params = Map.union (onlyJusts env) (onlyJusts $ spec)
  return (params, Map.keys $ Map.difference params (onlyJusts env))

getParameters :: String -> ParamSpec -> IO Params
getParameters name spec = do
  (params, usedDefaults) <- getParametersAndUsedDefaults spec

  when (Map.size params < length (names spec)) $ do
    writeLog "Missing the following mandatory parameters:"
    printIndentedList 2 (names spec \\ Map.keys params)
    newline
    exitFailure

  writeLog $ "Using the following " ++ name ++ " parameters:"
  let f (k, v) = (k, bolded v ++ if elem k usedDefaults
                                 then dimmed " (default)"
                                 else "")
  putStrLn . unlines . tabulate . map f $ Map.assocs params

  return params
