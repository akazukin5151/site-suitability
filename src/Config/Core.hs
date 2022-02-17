{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Core where

import Preprocessing.Combined (
  cropThenAverageRasters,
  cropThenUnionRasters,
  vectorProximityFromFiles, residentialProximity
 )
import Preprocessing.Core.Raster (
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
                   | ResidentialProximity
                   | VectorProximity
                   deriving (Generic, Show)

instance ToJSON PrepFunctions
instance FromJSON PrepFunctions

-- | PrepFunctions -> border -> [input] -> output -> IO preprocessed
evalPrepF :: PrepFunctions -> String -> [String] -> String -> IO String
evalPrepF CropThenAverageRasters = cropThenAverageRasters
evalPrepF CropThenUnionRasters   = cropThenUnionRasters
evalPrepF Slope                  = slopeFromElevation
evalPrepF Aspect                 = aspectFromElevation
evalPrepF ResidentialProximity   = residentialProximity
evalPrepF VectorProximity        = vectorProximityFromFiles

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

-- | Custom Parser for CriterionConfig and ConstraintConfig
-- Only notable thing it does is to allow the elements of the 'inputs' field (an array)
-- to be either a string or an object. See the docs for parseInputConfig
--
-- This is to increase the ergonomics of specifying paths, because paths are much
-- more common than requirement outputs, but using an object (enum) to differentiate
-- between paths and requirement ouputs is still the best choice because:
-- 1) Adding a new field can cause confusion. A prep/std function might ignore one
--    or the other. If the function expects one file but both fields are non-empty,
--    what should it do?
-- 2) Forcing array-of-objects makes it tedious to write a config with very little
--    or even no dependencies on requirement outputs
-- 3) Embedding the info in-band (eg, "path:../data/x.tif" and "out:x.tif") is not
--    type safe and hard to parse. Need to choose a separator that cannot be
--    part of a directory path
-- 4) The status quo (requirement outputs look like "out/suh/preprocessed/x.tif")
--    is not ideal because the path that doesn't exist prior to running the program.
--    The full path is also a leaky abstraction - it's debatable whether it should
--    be in the preprocessed dir, makes it too easy to write the wrong dir, and
--    if the code changes the out dir, the configs have to change as well.
--    The parent directories should be irrelevant; only the file names need to match.
-- 5) Encapsulation and flexibility: the prep/std function does not need to care
--    whether it should run a requirement -- opens up the possibility for a
--    function to take multiple files from both path and requirement output
-- If the element is a string, interpret it as a path
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
      parseFail $ "parsing an input field failed, expected 'Path' or 'RequireOutput', but encountered '" <> invalid <> "'"
parseInputConfig invalid =
  prependFailure "parsing inputs field failed, " $
    typeMismatch "Object or String" invalid

