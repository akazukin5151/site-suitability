module Preprocessing.Core.Raster where

import Control.Arrow ((<<<))
import Core (Layer (path), Raster (Raster), Vector (Vector), rasterCalculator)
import Preprocessing.Core (stepWrapper)
import System.FilePath ((</>))
import Utils (
  ShouldRemoveStepDir (RemoveStepDir),
  guardFileF,
  quoteDouble,
  runCmd,
 )

unionRastersIfMultiple :: (Raster -> IO a) -> FilePath -> [Raster] -> IO a
unionRastersIfMultiple func step_dir is = do
  land_use_in <-
    case is of
      [x] -> pure x
      xs  -> unionRasters xs $ Raster $ step_dir </> "land_use_in.tif"
  func land_use_in

cropRasterWithBorder :: Vector -> Raster -> Raster -> IO Raster
cropRasterWithBorder (Vector bf) (Raster i) (Raster out) = do
  guardFileF Raster out $
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

cropRasterWithBorderExtents :: Vector -> Raster -> Raster -> IO Raster
cropRasterWithBorderExtents bf (Raster i) (Raster out) = do
  -- TODO get extents automatically
  guardFileF Raster out $
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

averageRaster :: [Raster] -> Raster -> IO Raster
averageRaster = rasterCalculator calc_expr
  where
    add_expr letters_used = foldr1 (\a b -> a <> "+" <> b) letters_used
    calc_expr letters_used = "(" <> add_expr letters_used <> ")/2"

unionRasters :: [Raster] -> Raster -> IO Raster
unionRasters is (Raster out) = do
  let inputs = map (quoteDouble <<< path) is
  guardFileF Raster out $
    runCmd "gdal_merge.py" $
      [ "-of"
      , "GTiff"
      , "-o"
      , quoteDouble out
      ]
        <> inputs

gdaldem :: String
        -> [String]
        -> Raster
        -> Raster
        -> IO Raster
gdaldem cmd extra (Raster i) (Raster out) = do
  guardFileF Raster out $
    runCmd "gdaldem" $
      [ cmd
      , quoteDouble i
      , quoteDouble out
      , "-of"
      , "GTiff"
      , "-b"
      , "1"
      ]
        <> extra

slopeCmd :: Raster -> Raster -> IO Raster
slopeCmd = gdaldem "slope" ["-s", "111000.0", "-compute_edges"]

slopeFromElevation :: a -> [Raster] -> Raster -> IO Raster
slopeFromElevation _ is out =
  -- If multiple files given, union the rasters then pass to slopeCmd
  stepWrapper RemoveStepDir "slopeFromElevation"
    (\step_dir -> unionRastersIfMultiple (`slopeCmd` out) step_dir is)

aspectCmd :: Raster -> Raster -> IO Raster
aspectCmd = gdaldem "aspect" ["-z", "111000"]

aspectFromElevation :: a -> [Raster] -> Raster -> IO Raster
aspectFromElevation _ is out =
  -- If multiple files given, union the rasters then pass to aspectCmd
  stepWrapper RemoveStepDir "aspectFromElevation"
    (\step_dir -> unionRastersIfMultiple (`aspectCmd` out) step_dir is)

rasterProximity :: Raster -> Raster -> IO Raster
rasterProximity (Raster i) (Raster out) = do
  guardFileF Raster out $
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

reprojectRaster :: Raster -> Raster -> IO Raster
reprojectRaster (Raster i) (Raster out) = do
  guardFileF Raster out $
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
