{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.CriteriaConfig where

import Analysis (rangeStandardize, reverseRangeStandardize, standardize, suhSigmoid, gaussian, standardizeQGIS, linearClampedInc, linearClampedDec)
import GHC.Generics (Generic)
import Data.Aeson ( FromJSON (parseJSON), ToJSON (toEncoding), genericParseJSON, genericToEncoding, Value (Object), (.:), (.:?) )
import Core (Direction (LessBetter, MoreBetter), customOptions)
import Data.Aeson.Types (Parser, Array, prependFailure, typeMismatch)
import Data.Vector (toList)
import Config.Core (RequireConfig, InputConfig, PrepFunctions, parseInputConfig)


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
