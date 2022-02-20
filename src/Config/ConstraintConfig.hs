{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.ConstraintConfig where

import Config.Adapter (aspectConstraint', elevationConstraint', residentialConstraint', slopeConstraint', vectorConstraintFromFiles')
import Config.Core (InputConfig, RequireConfig, parseInputConfig)
import Core (AspectData, ConstraintData, customOptions)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), Value (Object), genericParseJSON, genericToEncoding, (.:), (.:?))
import Data.Aeson.Types (Array, Parser, prependFailure, typeMismatch)
import Data.Vector (toList)
import GHC.Generics (Generic)

data ConstraintConfig = ConstraintConfig
  { c_name    :: String
  , c_inputs  :: [InputConfig]
  , c_output  :: String
  , c_func    :: ConstraintFunction
  , c_require :: Maybe RequireConfig
  }
  deriving (Generic, Show)

instance ToJSON ConstraintConfig where
  toEncoding = genericToEncoding customOptions

instance FromJSON ConstraintConfig where
  -- TODO duplicated code?
  parseJSON (Object obj) = do
    array <- (obj .: "c_inputs" :: Parser Array)
    x <- traverse parseInputConfig array
    let c_inputs_ = toList x

    c_name_    <- obj .: "c_name"
    c_output_  <- obj .: "c_output"
    c_func_    <- obj .: "c_func"
    c_require_ <- obj .:? "c_require"

    pure $
      ConstraintConfig
        { c_name    = c_name_
        , c_inputs  = c_inputs_
        , c_output  = c_output_
        , c_func    = c_func_
        , c_require = c_require_
        }
  parseJSON invalid =
    prependFailure "parsing CriterionConfig failed, " $
      typeMismatch "Object" invalid

data ConstraintFunction = ResidentialConstraint ConstraintData
                        | VectorConstraint ConstraintData
                        | ElevationConstraint ConstraintData
                        | AspectConstraint AspectData
                        | SlopeConstraint ConstraintData
                        deriving (Generic, Show)

instance ToJSON ConstraintFunction where
  toEncoding = genericToEncoding customOptions

instance FromJSON ConstraintFunction where
  parseJSON = genericParseJSON customOptions

evalConstraintF :: ConstraintFunction -> String -> String -> [String] -> FilePath -> IO FilePath
evalConstraintF (ResidentialConstraint c) = residentialConstraint' c
evalConstraintF (VectorConstraint c)      = vectorConstraintFromFiles' c
evalConstraintF (ElevationConstraint c)   = elevationConstraint' c
evalConstraintF (AspectConstraint d)      = aspectConstraint' d
evalConstraintF (SlopeConstraint c)       = slopeConstraint' c
