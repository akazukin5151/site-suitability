module Preprocessing.Core.Vector where

import Core (Raster (Raster), Vector (Vector))
import System.FilePath ((</>))
import Utils (
  guardFileF,
  quoteDouble,
  runCmd,
 )

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

bufferBorder :: String -> Vector -> IO Vector
bufferBorder out_dir border = do
  let reproj_out_ = Vector $ out_dir </> "border_reproj.shp"
  reproj_out <- reprojectVector border reproj_out_ "EPSG:3857"

  let border_buff_out = Vector $ out_dir </> "border_reproj_buff.shp"
  bufferVector 100000 reproj_out border_buff_out

rasterizeVector :: Vector -> Raster -> IO Raster
rasterizeVector (Vector i) (Raster out) = do
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
