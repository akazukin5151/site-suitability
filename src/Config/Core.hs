{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Core where

import Preprocessing.Combined (
  cropThenAverageRasters,
  cropThenUnionRasters,
  vectorProximityFromFiles, residentialProximityNew
 )
import Preprocessing.Core (
  aspectFromElevation,
  slopeFromElevation,
 )
import Utils (Require (..))
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON (parseJSON), ToJSON (toEncoding), genericParseJSON, defaultOptions, Options (sumEncoding), SumEncoding (TaggedObject, UntaggedValue), genericToEncoding, Value (Object, String), (.:) )
import Data.Aeson.Types (Parser, parseFail, prependFailure, typeMismatch)
import Data.Text (unpack)

data PrepFunctions = CropThenAverageRasters
                   | CropThenUnionRasters
                   | Slope
                   | Aspect
                   | ResidentialProximityNew
                   | VectorProximity
                   deriving (Generic, Show)

instance ToJSON PrepFunctions
instance FromJSON PrepFunctions

-- | PrepFunctions -> border -> [input] -> output -> IO preprocessed
evalPrepF :: PrepFunctions -> String -> [String] -> String -> IO String
evalPrepF CropThenAverageRasters  = cropThenAverageRasters
evalPrepF CropThenUnionRasters    = cropThenUnionRasters
evalPrepF Slope                   = slopeFromElevation
evalPrepF Aspect                  = aspectFromElevation
evalPrepF ResidentialProximityNew = residentialProximityNew
evalPrepF VectorProximity         = vectorProximityFromFiles

data RequireConfig =
  RequireConfig { r_name   :: String
                , r_inputs :: [String]
                , r_output :: String
                , r_prep_f :: PrepFunctions
                }
                deriving (Generic, Show)

instance ToJSON RequireConfig where
  toEncoding = genericToEncoding $ defaultOptions { sumEncoding = UntaggedValue }

instance FromJSON RequireConfig where
  parseJSON = genericParseJSON $ defaultOptions { sumEncoding = UntaggedValue }

evalRequire :: Maybe RequireConfig -> Maybe Require
evalRequire Nothing   = Nothing
evalRequire (Just rc) = Just $ Require { _r_name   = r_name rc
                                       , _r_inputs = r_inputs rc
                                       , _r_output = r_output rc
                                       , _r_prep_f = evalPrepF $ r_prep_f rc
                                       }

data InputConfig = PathConfig String
                 | RequireOutputConfig String
                 deriving (Generic, Show)

instance ToJSON InputConfig where
  toEncoding =
    genericToEncoding
      defaultOptions { sumEncoding = TaggedObject "type" "string" }

instance FromJSON InputConfig where
  parseJSON =
    genericParseJSON
      defaultOptions { sumEncoding = TaggedObject "type" "string" }

-- | If the element is a string, interpret it as a path
-- If it is an object, and the 'type' field is 'Path', interpret as path
-- if the 'type' field is 'RequireOutput', interpret as requirement output
-- if the 'type' field is unknown or it is not an object, fail
parseInputConfig :: Value -> Parser InputConfig
parseInputConfig (String s) = pure $ PathConfig $ unpack s
parseInputConfig (Object o) = do
  type_   <- (o .: "type" :: Parser String)
  string_ <- (o .: "string" :: Parser String)
  case type_ of
    "Path"          -> pure $ PathConfig string_
    "RequireOutput" -> pure $ RequireOutputConfig string_
    invalid ->
      parseFail $ "parsing an input in CriterionConfig failed, expected 'Path' or 'RequireOutput', but encountered '" <> invalid <> "'"
parseInputConfig invalid =
  prependFailure "parsing inputs in CriterionConfig failed, " $
    typeMismatch "Object or String" invalid

