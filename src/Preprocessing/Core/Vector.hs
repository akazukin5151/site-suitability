module Preprocessing.Core.Vector where

import System.Directory (removePathForcibly)
import Utils (
  ShouldRemoveStepDir (DontRemoveStepDir),
  guardFile,
  quoteDouble,
  quoteSingle,
  runCmd, readCmd, guardFile'
 )
import System.FilePath ((</>))
import GHC.IO.Exception ( ExitCode(ExitFailure, ExitSuccess) )
import Preprocessing.Core (stepWrapper)

cropVectorWithBorder :: String -> String -> String -> IO String
cropVectorWithBorder bf i out = do
  guardFile' out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:clip"
      , "--"
      , "OVERLAY=" <> quoteDouble bf
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      ]

reprojectVector :: String -> String -> String -> IO String
reprojectVector i out projection = do
  guardFile' out $
    runCmd
      "ogr2ogr"
      [ "-t_srs"
      , projection
      , quoteDouble out
      , quoteDouble i
      ]

filterVectorByField :: String -> String -> String -> String -> IO String
filterVectorByField fname fvalue i out = do
  guardFile' out $
    runCmd
      "ogr2ogr"
      [ "-where"
      , quoteDouble (quoteDouble fname <> "=" <> quoteSingle fvalue)
      , quoteDouble out
      , quoteDouble i
      ]

unionVectors :: String -> String -> String -> IO String
unionVectors i1 i2 out = do
  guardFile' out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:union"
      , "--"
      , "INPUT=" <> quoteDouble i1
      , "OVERLAY=" <> quoteDouble i2
      , "OUTPUT=" <> quoteDouble out
      ]

dissolveVector :: String -> String -> IO String
dissolveVector i out = do
  guardFile' out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:dissolve"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      ]

bufferVector :: Double -> String -> String -> IO String
bufferVector dist i out = do
  guardFile' out $
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

addDummyField :: String -> String -> IO String
addDummyField i out = do
  guardFile' out $
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

removeFields :: String -> String -> IO String
removeFields i out = do
  guardFile' out $
    runCmd
      "qgis_process"
      [ "run"
      , "native:retainfields"
      , "--"
      , "INPUT=" <> quoteDouble i
      , "OUTPUT=" <> quoteDouble out
      , "FIELDS=\"id\""
      ]

vectorize :: FilePath -> FilePath -> IO FilePath
vectorize i out = do
  guardFile' out $
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

extractVectorAttribute :: String -> String -> String -> FilePath -> IO FilePath
extractVectorAttribute field value i out = do
  guardFile' out $
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

vectorDifference :: String -> String -> FilePath -> IO FilePath
vectorDifference i ov out = do
  guardFile' out $ do
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

bufferBorder :: FilePath -> String -> IO String
bufferBorder out_dir border = do
  let reproj_out_ = out_dir </> "border_reproj.shp"
  reproj_out <- reprojectVector border reproj_out_ "EPSG:3857"

  let border_buff_out = out_dir </> "border_reproj_buff.shp"
  bufferVector 100000 reproj_out border_buff_out

rasterizePower :: String -> String -> IO String
rasterizePower i out = do
  guardFile' out $
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
