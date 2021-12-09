{-# LANGUAGE DeriveGeneric #-}

module Config.Combined where

import Config.CriteriaConfig
    ( evalStdF,
      CriterionConfig(name, inputs, output, prep_f, std_f, weight,
                      require) )
import Config.ConstraintConfig
    ( evalConstraintF,
      ConstraintConfig(c_name, c_inputs, c_output, c_func, c_require) )
import GHC.Generics (Generic)
import Data.Aeson
    ( FromJSON(parseJSON),
      ToJSON(toEncoding),
      genericParseJSON,
      genericToEncoding )
import Utils (Criterion(..), Constraint (..), Input (RequireOutput, Path))
import Config.Core (InputConfig (PathConfig, RequireOutputConfig), evalRequire, evalPrepF)
import Core (customOptions)

data Config =
  Config { criteria :: [CriterionConfig]
         , constraints :: [ConstraintConfig]
         }
  deriving (Generic, Show)

instance ToJSON Config where
  toEncoding = genericToEncoding customOptions

instance FromJSON Config where
  parseJSON = genericParseJSON customOptions

configToCriteria :: Config -> ([Criterion], [Constraint])
configToCriteria Config {criteria = ca, constraints = co} =
  (map f ca, map g co)
  where
    f :: CriterionConfig -> Criterion
    f cc = Criterion { _name   = name cc
                     , _inputs = evalInput <$> inputs cc
                     , _output = output cc
                     , _prep_f = evalPrepF $ prep_f cc
                     , _std_f  = evalStdF $ std_f cc
                     , _result = Nothing
                     , _weight = weight cc
                     , _require = evalRequire $ require cc
                     }
    g :: ConstraintConfig -> Constraint
    g cc = Constraint { _c_name = c_name cc
                      , _c_inputs = evalInput <$> c_inputs cc
                      , _c_output = c_output cc
                      , _c_func = evalConstraintF $ c_func cc
                      , _c_require = evalRequire $ c_require cc
                      }

evalInput :: InputConfig -> Input
evalInput (PathConfig s)          = Path s
evalInput (RequireOutputConfig o) = RequireOutput o
