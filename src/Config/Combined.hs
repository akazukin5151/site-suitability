{-# LANGUAGE DeriveGeneric #-}

module Config.Combined where

import Config.ConstraintConfig (
  ConstraintConfig (c_func, c_inputs, c_name, c_output, c_require),
  evalConstraintF,
 )
import Config.Core (InputConfig (PathConfig, RequireOutputConfig), evalPrepF, evalRequire)
import Config.CriteriaConfig (
  CriterionConfig (
    inputs,
    name,
    output,
    prep_f,
    require,
    std_f,
    weight
  ),
  evalStdF,
 )
import Core (Vector (Vector), customOptions)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding),
  genericParseJSON,
  genericToEncoding,
 )
import GHC.Generics (Generic)
import Utils (Constraint (..), Criterion (..), Input (Path, RequireOutput))

data Config = Config
  { study_area  :: StudyArea
  , criteria    :: [CriterionConfig]
  , constraints :: [ConstraintConfig]
  }
  deriving (Generic, Show)

instance ToJSON Config where
  toEncoding = genericToEncoding customOptions

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions

data StudyArea = StudyArea {file :: String}
  deriving (Generic, Show)

instance ToJSON StudyArea where
  toEncoding = genericToEncoding customOptions

instance FromJSON StudyArea where
  parseJSON = genericParseJSON customOptions

configToCriteria :: Config -> (Vector, [Criterion], [Constraint])
configToCriteria Config {study_area = sa, criteria = ca, constraints = co} =
  (Vector $ file sa, map f ca, map g co)
  where
    f :: CriterionConfig -> Criterion
    f cc =
      Criterion
        { _name    = name cc
        , _inputs  = evalInput <$> inputs cc
        , _output  = output cc
        , _prep_f  = evalPrepF $ prep_f cc
        , _std_f   = evalStdF $ std_f cc
        , _result  = Nothing
        , _weight  = weight cc
        , _require = evalRequire $ require cc
        }
    g :: ConstraintConfig -> Constraint
    g cc =
      Constraint
        { _c_name    = c_name cc
        , _c_inputs  = evalInput <$> c_inputs cc
        , _c_output  = c_output cc
        , _c_func    = evalConstraintF $ c_func cc
        , _c_require = evalRequire $ c_require cc
        }

evalInput :: InputConfig -> Input
evalInput (PathConfig s)          = Path s
evalInput (RequireOutputConfig o) = RequireOutput o
