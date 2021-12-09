{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Analysis (rangeStandardize, reverseRangeStandardize, standardize, suhSigmoid, gaussian, standardizeQGIS, linearClampedInc, linearClampedDec)
import Preprocessing.Combined (
  cropThenAverageRasters,
  cropThenUnionRasters,
  residentialProximity,
  vectorProximityFromFiles, vectorConstraintFromFiles, residentialConstraint, elevationConstraint, aspectConstraint, slopeConstraint, residentialProximityNew
 )
import Preprocessing.Core (
  aspectFromElevation,
  slopeFromElevation,
 )
import Utils (Criterion (..), Require (..), Constraint (..), Input (Path, RequireOutput))
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON (parseJSON), ToJSON (toEncoding), genericParseJSON, defaultOptions, Options (sumEncoding), SumEncoding (TaggedObject, UntaggedValue), genericToEncoding, Value (Object, String), (.:), (.:?) )
import Core (Direction (LessBetter, MoreBetter), ConstraintData, customOptions, AspectData)
import Data.Aeson.Types (Parser, Array, parseFail, prependFailure, typeMismatch)
import Data.Text (unpack)
import Data.Vector (toList)

data PrepFunctions = CropThenAverageRasters
                   | CropThenUnionRasters
                   | Slope
                   | Aspect
                   | ResidentialProximity
                   | ResidentialProximityNew
                   | VectorProximity
                   deriving (Generic, Show)

instance ToJSON PrepFunctions
instance FromJSON PrepFunctions

-- | PrepFunctions -> border -> [input] -> output -> IO preprocessed
evalPrepF :: PrepFunctions -> String -> [String] -> String -> IO String
evalPrepF CropThenAverageRasters    = cropThenAverageRasters
evalPrepF CropThenUnionRasters      = cropThenUnionRasters
evalPrepF Slope                     = slopeFromElevation
evalPrepF Aspect                    = aspectFromElevation
evalPrepF ResidentialProximity      = residentialProximity
evalPrepF ResidentialProximityNew      = residentialProximityNew
evalPrepF VectorProximity           = vectorProximityFromFiles

data StdFunctions = RangeLargerBetter
                  | RangeSmallerBetter
                  -- | A sigmoidal function from Suh
                  | SuhSigmoid SuhSigmoidData
                  -- | A linear interpolation between two given max and min values
                  | Linear LinearFunction
                  | Gaussian GaussianFunction
                  | Expr ExprFunction
                  deriving (Generic, Show)

instance ToJSON StdFunctions where
  toEncoding = genericToEncoding customOptions

instance FromJSON StdFunctions where
  parseJSON = genericParseJSON customOptions

data SuhSigmoidData =
  SuhSigmoidData { spread   :: Double
                 , midpoint :: Double
                 , divide   :: Maybe Double
                 }
                 deriving (Generic, Show)

instance ToJSON SuhSigmoidData
instance FromJSON SuhSigmoidData

-- | pre-conditions: clamp_left < clamp_right
data LinearFunction =
  LinearFunction { clamp_left :: Double
                 -- ^ For all values less than `clamp_left`, it will be standardized
                 -- to 0 if MoreBetter or 1 if LessBetter
                 , clamp_right :: Double
                 -- ^ For all values greater than `clamp_right`, it will be
                 -- standardized to 1 if MoreBetter or 0 if LessBetter
                 , direction :: Direction
                 -- ^ Whether the function is MoreBetter or LessBetter
                 -- MoreBetter = positive gradient; LessBetter = negative gradient
                 -- If MoreBetter:
                 --   { 0                              , x < clamp_left
                 --   ; 1                              , x > clamp_right
                 --   ; -1 / (clamp_right - clamp_left), clamp_left < x < clamp_right
                 --   }
                 -- If LessBetter:
                 --   { 0                             , x > clamp_right
                 --   ; 1                             , x < clamp_left
                 --   ; 1 / (clamp_right - clamp_left), clamp_left < x < clamp_right
                 --   }
                 }
  deriving (Generic, Show)

instance ToJSON LinearFunction
instance FromJSON LinearFunction

data GaussianFunction =
  GaussianFunction { peak_x :: Double
                   , g_midpoint :: Double
                   , g_divide :: Double
                   }
                   deriving (Generic, Show)

instance ToJSON GaussianFunction
instance FromJSON GaussianFunction

newtype ExprFunction = ExprFunction { expr :: String }
  deriving (Generic, Show)

instance ToJSON ExprFunction
instance FromJSON ExprFunction

evalStdF :: StdFunctions -> String -> String -> IO String
evalStdF RangeLargerBetter       = rangeStandardize
evalStdF RangeSmallerBetter      = reverseRangeStandardize
evalStdF (Linear lf)             = evalLinear lf
evalStdF (Gaussian g)            = evalGaussian g
evalStdF (Expr (ExprFunction s)) = standardize s
evalStdF (SuhSigmoid d)          =
  standardizeQGIS $ suhSigmoid (midpoint d) (spread d) (divide d)

evalLinear :: LinearFunction -> String -> String -> IO String
evalLinear lf = standardize $ f m c x1 x2
  where
    m        = (y2 - y1) / (x2 - x1)
    x1       = clamp_left lf
    x2       = clamp_right lf
    c        = y1 - m*x1
    (y1, y2, f) =
      case direction lf of
        MoreBetter -> (0, 1, linearClampedInc)
        LessBetter -> (1, 0, linearClampedDec)

evalGaussian :: GaussianFunction -> String -> String -> IO String
evalGaussian g = standardizeQGIS $ gaussian b c div'
  where
    div' = g_divide g
    b = peak_x g
    -- substitute x=midpoint, y=0.5 and solve for c
    -- `log` is the natural log
    c = sqrt ( -((m-b)**2) / (2 * log 0.5))
    m = g_midpoint g

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

data CriterionConfig =
  CriterionConfig { name    :: String
                  , inputs  :: [InputConfig]
                  , output  :: String
                  , prep_f  :: PrepFunctions
                  , std_f   :: StdFunctions
                  , weight  :: Double
                  , require :: Maybe RequireConfig
                  }
                  deriving (Generic, Show)

-- TODO: Do I need to write a custom implementation for this
-- if I already have one for FromJSON?
instance ToJSON CriterionConfig where
  toEncoding = genericToEncoding customOptions

instance FromJSON CriterionConfig where
  -- | Custom Parser for CriterionConfig
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
  parseJSON (Object obj) = do
    array <- (obj .: "inputs" :: Parser Array)
    x <- traverse parseInputConfig array
    let inputs_ = toList x

    -- no need to use custom sumEncoding for prep, std, and require
    name_    <- obj .: "name"
    output_  <- obj .: "output"
    prep_f_  <- obj .: "prep_f"
    std_f_   <- obj .: "std_f"
    weight_  <- obj .: "weight"
    require_ <- obj .:? "require"

    pure $ CriterionConfig
      { name    = name_
      , inputs  = inputs_
      , output  = output_
      , prep_f  = prep_f_
      , std_f   = std_f_
      , weight  = weight_
      , require = require_
      }
  parseJSON invalid =
    prependFailure "parsing CriterionConfig failed, " $
      typeMismatch "Object" invalid

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

data ConstraintConfig =
  ConstraintConfig  { c_name :: String
                    , c_inputs :: [InputConfig]
                    , c_output :: String
                    , c_func :: ConstraintFunction
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

    pure $ ConstraintConfig
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

evalConstraintF :: ConstraintFunction -> String -> [String] -> FilePath -> IO FilePath
evalConstraintF (ResidentialConstraint c) = residentialConstraint c
evalConstraintF (VectorConstraint c)      = vectorConstraintFromFiles c
evalConstraintF (ElevationConstraint c)   = elevationConstraint c
evalConstraintF (AspectConstraint d)      = aspectConstraint d
evalConstraintF (SlopeConstraint c)       = slopeConstraint c

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
