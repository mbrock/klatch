{-# LANGUAGE TupleSections #-}

module Klatch.Common.Params where

import Klatch.Common.Util

import Control.Monad             (when)
import Data.Map                  (Map)
import Data.List                 ((\\))
import Data.Text                 (Text)
import Options.Applicative.Utils (tabulate)
import System.Exit               (exitFailure)
import System.Posix.Env          (getEnv)

import qualified Data.Map  as Map
import qualified Data.Text as T

type ParamSpec = Map String (Maybe Text)
type Params = Map String Text

names :: ParamSpec -> [String]
names = Map.keys

getParametersAndUsedDefaults :: ParamSpec -> IO (Params, [String])
getParametersAndUsedDefaults spec = do
  env <- mapM (\k -> fmap ((k,) . fmap T.pack) (getEnv k)) (names spec)

  let provided = onlyJusts . Map.fromList $ env
      defaults = onlyJusts spec
      params = Map.union provided defaults

  return (params, Map.keys $ Map.difference params provided)

getParameters :: String -> ParamSpec -> IO Params
getParameters name spec = do
  (params, usedDefaults) <- getParametersAndUsedDefaults spec

  when (Map.size params < length (names spec)) $ do
    writeLog "Missing the following mandatory parameters:"
    printIndentedList 2 (names spec \\ Map.keys params)
    newline
    exitFailure

  writeLog $ "Using the following " ++ name ++ " parameters:"
  let f (k, v) = (k, bolded (T.unpack v) ++ indicateDefaultness k)
      indicateDefaultness s =
        if elem s usedDefaults
        then dimmed " (default)"
        else ""

  putStrLn . unlines . tabulate . map f $ Map.assocs params

  return params
