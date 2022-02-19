{-# LANGUAGE OverloadedStrings #-}

module Metadata where

import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Exit (ExitCode (ExitSuccess))
import Utils (readCmd)

import Data.Aeson (Value (Number, String), decode)
import Data.ByteString.Lazy.UTF8 (fromString)

import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth)
import Data.Scientific (Scientific)
import Data.Text (unpack)

getMinMax :: String -> IO (Scientific, Scientific)
getMinMax i = do
  (code, stdout, stderr) <- readCmd "gdalinfo" ["-mm", "-json", i]
  (min', max') <- case code of
    ExitFailure _ -> print stderr >> error "Failed to get min and max values"
    ExitSuccess   -> case parseMinMax stdout of
      Left x  -> error x
      Right x -> pure x
  pure (min', max')

parseMinMax :: String -> Either String (Scientific, Scientific)
parseMinMax stdout = do
  value <-
    case (decode (fromString stdout) :: Maybe Value) of
      Just x -> pure x
      _      -> Left "Failed to decode JSON"

  -- band contains an array, and `nth 0` takes the first item in the array
  fst_band <-
    case value ^? key "bands" . nth 0 of
      Just x -> pure x
      _      -> Left "Failed to get the first band"

  -- The metadata is more accurate than computed
  -- metadata is a nested dict, eg:
  -- "metadata": { "": {"STATISTICS_MINIMUM": "1"}  }
  -- but if it fails, fall back to computedMin and computedMax
  case fst_band ^? key "metadata" . key "" of
    Just metadata -> do
      mmin <-
        case metadata ^? key "STATISTICS_MINIMUM" of
          Just (String m) -> pure m
          _               -> Left "Failed to decode min"

      mmax <-
        case metadata ^? key "STATISTICS_MAXIMUM" of
          Just (String m) -> pure m
          _               -> Left "Failed to decode max"

      pure (read $ unpack mmin, read $ unpack mmax)
    _ -> do
      mmin <-
        case fst_band ^? key "computedMin" of
          Just (Number x) -> pure x
          _               -> Left "Failed to get metadata and computedMin"

      mmax <-
        case fst_band ^? key "computedMax" of
          Just (Number x) -> pure x
          _ -> Left "Failed to get metadata and computedMax"

      pure (mmin, mmax)
