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

-- | A GIS Layer. The only requirement is to be able to get the inner path.
-- This avoid pattern matching every time
class Layer a where
  path :: a -> String

instance Layer Raster where
  path (Raster r) = r

instance Layer Vector where
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
    -- extents optional
    ]

-- TODO: copied & modified from multiplyRasters
averageRastersQGIS :: [String] -> [Raster] -> Raster -> IO Raster
averageRastersQGIS extra is' (Raster out) = do
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
    calc_expr = "(" <> add_expr <> ")/" <> show (length is)
    add_expr = foldr1 (\a b -> a <> " + " <> b) layered

