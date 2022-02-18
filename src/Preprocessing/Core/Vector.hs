module Preprocessing.Core.Vector where

import System.Directory (removePathForcibly)
import Utils (
  ShouldRemoveStepDir (DontRemoveStepDir),
  guardFile,
  quoteDouble,
  quoteSingle,
  runCmd, readCmd, guardFileF
 )
import System.FilePath ((</>))
import GHC.IO.Exception ( ExitCode(ExitFailure, ExitSuccess) )
import Preprocessing.Core (stepWrapper)
import Core (Vector(Vector), Raster (Raster))

cropVectorWithBorder :: Vector -> Vector -> Vector -> IO Vector
cropVectorWithBorder (Vector bf) (Vector i) (Vector out) = do
  guardFileF Vector out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:clip"
      , "--"
      , "OVERLAY=" <> quoteDouble bf
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      ]

reprojectVector :: Vector -> Vector -> String -> IO Vector
reprojectVector (Vector i) (Vector out) projection = do
  guardFileF Vector out $
    runCmd
      "ogr2ogr"
      [ "-t_srs"
      , projection
      , quoteDouble out
      , quoteDouble i
      ]

filterVectorByField :: String -> String -> Vector -> Vector -> IO Vector
filterVectorByField fname fvalue (Vector i) (Vector out) = do
  guardFileF Vector out $
    runCmd
      "ogr2ogr"
      [ "-where"
      , quoteDouble (quoteDouble fname <> "=" <> quoteSingle fvalue)
      , quoteDouble out
      , quoteDouble i
      ]

unionVectors :: Vector -> Vector -> Vector -> IO Vector
unionVectors (Vector i1) (Vector i2) (Vector out) = do
  guardFileF Vector out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:union"
      , "--"
      , "INPUT=" <> quoteDouble i1
      , "OVERLAY=" <> quoteDouble i2
      , "OUTPUT=" <> quoteDouble out
      ]

dissolveVector :: Vector -> Vector -> IO Vector
dissolveVector (Vector i) (Vector out) = do
  guardFileF Vector out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:dissolve"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      ]

bufferVector :: Double -> Vector -> Vector -> IO Vector
bufferVector dist (Vector i) (Vector out) = do
  guardFileF Vector out $
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

addDummyField :: Vector -> Vector -> IO Vector
addDummyField (Vector i) (Vector out) = do
  guardFileF Vector out $
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

removeFields :: Vector -> Vector -> IO Vector
removeFields (Vector i) (Vector out) = do
  guardFileF Vector out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:retainfields"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      , "FIELDS=\"id\""
      ]

vectorize :: Raster -> Vector -> IO Vector
vectorize (Raster i) (Vector out) = do
  guardFileF Vector out $
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

extractVectorAttribute :: String -> String -> Vector -> Vector -> IO Vector
extractVectorAttribute field value (Vector i) (Vector out) = do
  guardFileF Vector out $
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

vectorDifference :: Vector -> Vector -> Vector -> IO Vector
vectorDifference (Vector i) (Vector ov) (Vector out) = do
  guardFileF Vector out $ do
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

bufferBorder :: String -> Vector -> IO Vector
bufferBorder out_dir border = do
  let reproj_out_ = Vector $ out_dir </> "border_reproj.shp"
  reproj_out <- reprojectVector border reproj_out_ "EPSG:3857"

  let border_buff_out = Vector $ out_dir </> "border_reproj_buff.shp"
  bufferVector 100000 reproj_out border_buff_out

rasterizePower :: Vector -> Raster -> IO Raster
rasterizePower (Vector i) (Raster out) = do
  guardFileF Raster out $
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
