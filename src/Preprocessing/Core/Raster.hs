module Preprocessing.Core.Raster where

import Core (rasterCalculator)
import Utils (
  quoteDouble,
  runCmd, guardFile'
 )

cropRasterWithBorder :: String -> String -> String -> IO String
cropRasterWithBorder bf i out = do
  guardFile' out $
    runCmd
      "gdalwarp"
      [ "-of"
      , "GTiff"
      , "-cutline"
      , quoteDouble bf
      , "-crop_to_cutline"
      , quoteDouble i
      , quoteDouble out
      ]

cropRasterWithBorderExtents :: String -> String -> String -> IO String
cropRasterWithBorderExtents bf i out = do
  -- TODO get extents automatically
  guardFile' out $
    runCmd
      "gdal_translate"
      [ "-projwin"
      , "-114.8154068"
      , "37.00458908"
      , "-109.0448761"
      , "31.32917023"
      , "-of"
      , "GTiff"
      , quoteDouble i
      , quoteDouble out
      ]

averageRaster :: [String] -> String -> IO String
averageRaster = rasterCalculator calc_expr
  where
    add_expr letters_used = foldr1 (\a b -> a <> "+" <> b) letters_used
    calc_expr letters_used = "(" <> add_expr letters_used <> ")/2"

unionRasters :: [String] -> String -> IO String
unionRasters is out = do
  let inputs = map quoteDouble is
  guardFile' out $
    runCmd "gdal_merge.py" $
      [ "-of"
      , "GTiff"
      , "-o"
      , quoteDouble out
      ] <> inputs

gdaldem :: String
        -> [String]
        -> String
        -> String
        -> IO String
gdaldem cmd extra i out = do
  guardFile' out $
    runCmd "gdaldem" $
      [ cmd
      , quoteDouble i
      , quoteDouble out
      , "-of"
      , "GTiff"
      , "-b"
      , "1"
      ] <> extra

slopeCmd :: String -> String -> IO String
slopeCmd = gdaldem "slope" ["-s", "111000.0", "-compute_edges"]

-- TODO: these functions only take exactly one input, but would fail when run
-- (potentially after other calculations)
-- ideally, the config should be validated first
-- but validation makes it more specific and less flexible
-- if it only accepts one input but someone in the future wants to change it to
-- accept multiple inputs, they would have more work
-- (as the validator needs to be changed too)
-- best to let it accept lists, but validate (ensure at runtime) it has only one item
slopeFromElevation :: a -> [String] -> String -> IO String
slopeFromElevation _ [is] = slopeCmd is

aspectCmd :: String -> String -> IO String
aspectCmd = gdaldem "aspect" ["-z", "111000"]

aspectFromElevation :: a -> [String] -> String -> IO String
aspectFromElevation _ [is] = aspectCmd is

rasterProximity :: String -> String -> IO String
rasterProximity i out = do
  guardFile' out $
    runCmd
      "gdal_proximity.py"
      [ "-srcband"
      , "1"
      , "-distunits"
      , "GEO"
      , "-ot"
      , "Float32"
      , "-of"
      , "GTiff"
      , quoteDouble i
      , quoteDouble out
      ]

reprojectRaster :: String -> FilePath -> IO FilePath
reprojectRaster i out = do
  guardFile' out $
    runCmd
    "gdalwarp"
    [ "-t_srs"
    , "EPSG:3857"
    , "-r"
    , "near"
    , "-of"
    , "GTiff"
    , quoteDouble i
    , quoteDouble out
    ]
