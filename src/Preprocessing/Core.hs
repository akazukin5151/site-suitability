module Preprocessing.Core where

import Control.Monad (when)
import Core (rasterCalculator)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import Utils (
  ShouldRemoveStepDir (RemoveStepDir, DontRemoveStepDir),
  guardFile,
  quoteDouble,
  quoteSingle,
  runCmd, readCmd
 )
import System.FilePath ((</>))
import GHC.IO.Exception

stepWrapper :: ShouldRemoveStepDir -> String -> (String -> IO a) -> IO a
stepWrapper should_remove f_name f = do
  let step_dir = "internal_steps_" <> f_name
  createDirectoryIfMissing True step_dir
  res <- f step_dir
  when (should_remove == RemoveStepDir) $
    removePathForcibly step_dir
  pure res

cropVectorWithBorder :: String -> String -> String -> IO String
cropVectorWithBorder bf i out = do
  guardFile out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:clip"
      , "--"
      , "OVERLAY=" <> quoteDouble bf
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      ]
  pure out

cropRasterWithBorder :: String -> String -> String -> IO String
cropRasterWithBorder bf i out = do
  guardFile out $
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
  pure out

cropRasterWithBorderExtents :: String -> String -> String -> IO String
cropRasterWithBorderExtents bf i out = do
  -- TODO get extents automatically
  guardFile out $
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
  pure out

averageRaster :: [String] -> String -> IO String
averageRaster = rasterCalculator calc_expr
  where
    add_expr letters_used = foldr1 (\a b -> a <> "+" <> b) letters_used
    calc_expr letters_used = "(" <> add_expr letters_used <> ")/2"

reprojectVector :: String -> String -> String -> IO String
reprojectVector i out projection = do
  guardFile out $
    runCmd
      "ogr2ogr"
      [ "-t_srs"
      , projection
      , quoteDouble out
      , quoteDouble i
      ]
  pure out

filterVectorByField :: String -> String -> String -> String -> IO String
filterVectorByField fname fvalue i out = do
  guardFile out $
    runCmd
      "ogr2ogr"
      [ "-where"
      , quoteDouble (quoteDouble fname <> "=" <> quoteSingle fvalue)
      , quoteDouble out
      , quoteDouble i
      ]
  pure out

unionVectors :: String -> String -> String -> IO String
unionVectors i1 i2 out = do
  guardFile out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:union"
      , "--"
      , "INPUT=" <> quoteDouble i1
      , "OVERLAY=" <> quoteDouble i2
      , "OUTPUT=" <> quoteDouble out
      ]
  pure out

dissolveVector :: String -> String -> IO String
dissolveVector i out = do
  guardFile out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:dissolve"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      ]
  pure out

unionRasters :: [String] -> String -> IO String
unionRasters is out = do
  let inputs = map quoteDouble is
  guardFile out $
    runCmd "gdal_merge.py" $
      [ "-of"
      , "GTiff"
      , "-o"
      , quoteDouble out
      ] <> inputs
  pure out

gdaldem :: String
        -> [String]
        -> String
        -> String
        -> IO String
gdaldem cmd extra i out = do
  guardFile out $
    runCmd "gdaldem" $
      [ cmd
      , quoteDouble i
      , quoteDouble out
      , "-of"
      , "GTiff"
      , "-b"
      , "1"
      ] <> extra
  pure out

slopeCmd :: String -> String -> IO String
slopeCmd = gdaldem "slope" ["-s", "111000.0", "-compute_edges"]

-- TODO: this is broken if in requirements
slopeFromElevation :: a -> [String] -> String -> IO String
slopeFromElevation _ [is] = slopeCmd is

aspectCmd :: String -> String -> IO String
aspectCmd = gdaldem "aspect" ["-z", "111000"]

aspectFromElevation :: a -> [String] -> String -> IO String
aspectFromElevation _ [is] = aspectCmd is

vectorizeLandUse :: String -> String -> IO String
vectorizeLandUse i out = do
  guardFile out $
    runCmd
      "gdal_polygonize.py"
      [ quoteDouble i
      , quoteDouble out
      , "-b"
      , "1"
      , "OUTPUT"
      , "DN" -- TODO: make this customisable
      ]
  pure out

bufferVector :: Double -> String -> String -> IO String
bufferVector dist i out = do
  guardFile out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:buffer"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "DISSOLVE=\"True\""
      , "DISTANCE=" <> show dist
      , "OUTPUT=" <> quoteDouble out
      ]
  pure out

addDummyField :: String -> String -> IO String
addDummyField i out = do
  guardFile out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:fieldcalculator"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "FIELD_NAME=\"DUMMY\""
      , "FIELD_TYPE=1"
      , "FORMULA=1"
      , "OUTPUT=" <> quoteDouble out
      ]
  pure out

rasterizePower :: String -> String -> IO String
rasterizePower i out = do
  guardFile out $
    runCmd
      "gdal_rasterize"
      [ "-a"
      , "dummy"
      , "-tr"
      , "100.0"
      , "100.0"
      , "-ot"
      , "Float32"
      , "-of"
      , "GTiff"
      , quoteDouble i
      , quoteDouble out
      ]
  pure out

rasterProximity :: String -> String -> IO String
rasterProximity i out = do
  guardFile out $
    runCmd
      "gdal_proximity.py"
      [ "-srcband"
      , "1"
      , "-distunits"
      , "PIXEL"
      , "-nodata"
      , "0.0"
      , "-ot"
      , "Float32"
      , "-of"
      , "GTiff"
      , quoteDouble i
      , quoteDouble out
      ]
  pure out

removeFields :: String -> String -> IO String
removeFields i out = do
  guardFile out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:retainfields"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      , "FIELDS=\"id\""
      ]
  pure out

vectorize :: FilePath -> FilePath -> IO FilePath
vectorize i out = do
  guardFile out $
    runCmd
      "gdal_polygonize.py"
      [ quoteDouble i
      , quoteDouble out
      , "-b"
      , "1"
      , "-f"
      , "\"GPKG\""
      , "OUTPUT"
      , "DN"
      ]
  pure out

extractVectorAttribute :: String -> String -> String -> FilePath -> IO FilePath
extractVectorAttribute field value i out = do
  guardFile out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:extractbyattribute"
      , "--"
      , "FIELD=" <> quoteSingle field
      , "OPERATOR=0"  -- '='
      , "VALUE=" <> quoteSingle value
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      ]
  pure out

vectorDifference :: String -> String -> FilePath -> IO FilePath
vectorDifference i ov out = do
  guardFile out $ do
    (status, _, _) <- readCmd
      "qgis_process"
      [ "run"
      , "native:difference"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "OVERLAY=" <> quoteDouble ov
      , "OUTPUT=" <> quoteDouble out
      ]
    case status of
      ExitSuccess -> pure ()
      ExitFailure _ -> do
        (step_dir, fixed_ov) <- fixOverlayGeom ov
        runCmd
          "qgis_process"
          [ "run"
          , "native:difference"
          , "--"
          , "INPUT=" <> quoteDouble i
          , "OVERLAY=" <> quoteDouble fixed_ov
          , "OUTPUT=" <> quoteDouble out
          ]
        removePathForcibly step_dir
  pure out
    where
      fixOverlayGeom ov' =
        stepWrapper DontRemoveStepDir "fixOverlayGeom"
          (\step_dir -> do
              let fixed_out = step_dir </> "_fixed.shp"
              guardFile fixed_out $
                runCmd
                  "qgis_process"
                  [ "run"
                  , "native:fixgeometries"
                  , "--"
                  , "INPUT=" <> quoteDouble ov'
                  , "OUTPUT=" <> quoteDouble fixed_out
                  ]
              pure (step_dir, fixed_out)
            )

bufferBorder :: FilePath -> String -> IO String
bufferBorder out_dir border = do
  let reproj_out_ = out_dir </> "border_reproj.shp"
  reproj_out <- reprojectVector border reproj_out_ "EPSG:3857"

  let border_buff_out = out_dir </> "border_reproj_buff.shp"
  bufferVector 100000 reproj_out border_buff_out
