{-# LANGUAGE DeriveGeneric #-}

module Core where

import Data.Aeson (
  FromJSON (parseJSON),
  Options (sumEncoding),
  SumEncoding (ObjectWithSingleField, TaggedObject),
  ToJSON (toEncoding),
  defaultOptions,
  genericParseJSON,
  genericToEncoding,
 )
import GHC.Generics (Generic)
import Utils (guardFileF, quoteDouble, quoteSingle, runCmd)

newtype Raster = Raster String
newtype Vector = Vector String

-- Handy functions to get the inner path, to avoid pattern matching every time
class Path a where
  path :: a -> String

instance Path Raster where
  path (Raster r) = r

instance Path Vector where
  path (Vector r) = r

data Direction = MoreBetter | LessBetter
  deriving (Generic, Show)

instance ToJSON Direction where
  toEncoding =
    genericToEncoding $
      defaultOptions {sumEncoding = ObjectWithSingleField}

instance FromJSON Direction where
  parseJSON =
    genericParseJSON $
      defaultOptions {sumEncoding = ObjectWithSingleField}

data ConstraintData = ConstraintData
  { distance    :: Double
  , c_direction :: Direction
  }
  deriving (Generic, Show)

instance ToJSON ConstraintData where
  toEncoding = genericToEncoding customOptions

instance FromJSON ConstraintData where
  parseJSON = genericParseJSON customOptions

data AspectData = AspectData
  { limit1 :: Double
  , limit2 :: Double
  }
  deriving (Generic, Show)

instance ToJSON AspectData where
  toEncoding = genericToEncoding customOptions

instance FromJSON AspectData where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions {sumEncoding = TaggedObject "function" "args"}

multiplyRasters :: [String] -> [Raster] -> Raster -> IO Raster
multiplyRasters extra is' (Raster out) = do
  guardFileF Raster out $
    runCmd "qgis_process" $
      [ "run"
      , "qgis:rastercalculator"
      , "--"
      , "CELLSIZE=0"
      , "EXPRESSION=" <> quoteSingle calc_expr
      , "OUTPUT=" <> quoteSingle out
      ]
        <> input_cmds
        <> extra
  where
    is = path <$> is'
    input_cmds = ["LAYERS=" <> quoteSingle layer | layer <- is]
    layered = [quoteDouble (x <> "@1") | x <- is]
    calc_expr =
      foldr1 (\a b -> a <> " * " <> b) layered

finalRasterCalculator :: [Raster] -> Raster -> IO Raster
finalRasterCalculator =
  multiplyRasters
    [ "CRS='EPSG:4326'"
    -- TODO get extents; got it from insolation, which should be same as border
    -- extents optional
    -- , "EXTENT='-114.808333333,-109.050000000,31.333333333,37.000000000'"
    ]

rasterCalculator :: ([String] -> String) -> [Raster] -> Raster -> IO Raster
rasterCalculator calc_expr is (Raster out) = do
  guardFileF Raster out $
    runCmd "gdal_calc.py" $
      input_cmds
        <> [ "--outfile"
           , quoteDouble out
           , "--calc=" <> quoteDouble (calc_expr letters_used)
           ]
  where
    input_cmds = zipWith f ['A'..'Z'] (path <$> is)
    f letter i = "-" <> [letter] <> " " <> quoteDouble i
    -- Need to convert from String to [String] (but each element is a single character)
    letters_used = map (: []) $ take (length is) ['A'..'Z']
