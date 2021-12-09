{-# LANGUAGE DeriveGeneric #-}
module Core where

import Utils ( quoteSingle, quoteDouble, runCmd, guardFile' )
import GHC.Generics (Generic)
import Data.Aeson
    ( genericParseJSON,
      defaultOptions,
      genericToEncoding,
      FromJSON(parseJSON),
      Options(sumEncoding),
      SumEncoding(ObjectWithSingleField, TaggedObject),
      ToJSON(toEncoding) )

data Direction = MoreBetter | LessBetter
  deriving (Generic, Show)

instance ToJSON Direction where
  toEncoding =
    genericToEncoding
      $ defaultOptions { sumEncoding = ObjectWithSingleField }

instance FromJSON Direction where
  parseJSON =
    genericParseJSON
      $ defaultOptions { sumEncoding = ObjectWithSingleField }

data ConstraintData =
  ConstraintData { distance :: Double
                 , c_direction :: Direction
                 }
  deriving (Generic, Show)

instance ToJSON ConstraintData where
  toEncoding = genericToEncoding customOptions

instance FromJSON ConstraintData where
  parseJSON = genericParseJSON customOptions

data AspectData =
  AspectData { limit1 :: Double
             , limit2 :: Double
             }
  deriving (Generic, Show)

instance ToJSON AspectData where
  toEncoding = genericToEncoding customOptions

instance FromJSON AspectData where
  parseJSON = genericParseJSON customOptions

customOptions :: Options
customOptions = defaultOptions { sumEncoding = TaggedObject "function" "args" }

multiplyRasters :: [String] -> [String] -> String -> IO String
multiplyRasters extra is out = do
  guardFile' out $
    runCmd "qgis_process" $
      [ "run"
      , "qgis:rastercalculator"
      , "--"
      , "CELLSIZE=0"
      , "EXPRESSION=" <> quoteSingle calc_expr
      , "OUTPUT=" <> quoteSingle out
      ] <> input_cmds <> extra
    where
      input_cmds = ["LAYERS=" <> quoteSingle layer | layer <- is]
      layered = [quoteDouble (x <> "@1") | x <- is]
      calc_expr =
        foldr1 (\a b -> a <> " * " <> b) layered

finalRasterCalculator :: [String] -> String -> IO String
finalRasterCalculator =
  multiplyRasters
      [ "CRS='EPSG:4326'"
      -- TODO get extents; got it from insolation, which should be same as border
      -- extents optional
      --, "EXTENT='-114.808333333,-109.050000000,31.333333333,37.000000000'"
      ]

rasterCalculator :: ([String] -> String) -> [String] -> String -> IO String
rasterCalculator calc_expr is out = do
  guardFile' out $
    runCmd "gdal_calc.py" $
      input_cmds
        <> [ "--outfile"
           , quoteDouble out
           , "--calc=" <> quoteDouble (calc_expr letters_used)
           ]
    where
      alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      input_cmds = zipWith f alphabet is
      f letter i = "-" <> [letter] <> " " <> quoteDouble i
      -- Need to convert from String to [String] (but each element is a single character)
      letters_used = map (:[]) $ take (length is) alphabet
