module Preprocessing.Combined where

import Analysis (standardize)
import Control.Monad (void, zipWithM_)
import Core (Layer (path), Raster (Raster), Vector (Vector), averageRastersQGIS)
import Preprocessing.Core (stepWrapper)
import Preprocessing.Core.Raster (
  cropRasterWithBorder,
  cropRasterWithBorderExtents,
  rasterProximity,
  reprojectRaster,
  unionRasters,
  unionRastersIfMultiple,
 )
import Preprocessing.Core.Vector (
  addDummyField,
  bufferBorder,
  cropVectorWithBorder,
  dissolveVector,
  rasterizePower,
  removeFields,
  reprojectVector,
  unionVectors,
 )
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import Utils (
  ShouldRemoveStepDir (DontRemoveStepDir, RemoveStepDir),
  appendFilename,
  foldM1,
  guardFileF,
  sequentialFilenames,
 )

unionAllVectors :: String -> [Vector] -> Vector -> IO Vector
unionAllVectors config_name is out = do
  -- Remove all fields first
  (_, final_vec) <- foldM1 unionStep $ zip [0 ..] is
  res <- dissolveVector final_vec out
  removePathForcibly step_dir_
  pure res
  where
    f_name = "unionStep"
    step_dir_ = "out" </> config_name </> "internal_steps_" <> f_name

    -- Union two vectors with the wrapper (puts the result in another dir)
    unionStep :: (Int, Vector) -> (Int, Vector) -> IO (Int, Vector)
    unionStep (this_idx, i1) (next_idx, i2) =
      stepWrapper DontRemoveStepDir config_name f_name
        ( \step_dir -> do
            let no_fields_i1_ = Vector $ appendFilename "_no_fields" $ path i1
            i1_no_fields <- removeFields i1 no_fields_i1_

            let no_fields_i2_ = Vector $ appendFilename "_no_fields" $ path i2
            i2_no_fields <- removeFields i2 no_fields_i2_

            let step_output = Vector $ step_dir </> ("step_" <> show this_idx <> ".shp")
            res <- unionVectors i1_no_fields i2_no_fields step_output
            pure (next_idx, res)
        )

residentialProximity :: String -> Vector -> [Raster] -> Raster -> IO Raster
residentialProximity config_name border_output_file is (Raster out) = do
  border_buff <- bufferBorder "out" border_output_file
  guardFileF Raster out $
    void $
      stepWrapper RemoveStepDir config_name "residentialProximity"
        ( \step_dir -> do
            let land_use_out_ = Raster $ step_dir </> "land_use_out.tif"
            -- union rasters if multiple before running cropRasterWithBorder
            let func land_use_in =
                  cropRasterWithBorder border_buff land_use_in land_use_out_
            land_use_out <- unionRastersIfMultiple func step_dir is

            let residential_out_ = Raster $ step_dir </> "residential_only.tif"
            let expr =
                  "0*logical_or(logical_or(A!=22, A!=23), A!=24) + 1*logical_or(logical_or(A==22, A==23), A==24)"
            residential_out <- standardize expr land_use_out residential_out_

            -- reproject to meters
            let reproj_out_ = Raster $ step_dir </> "residential_reproj.tif"
            reproj_out <- reprojectRaster residential_out reproj_out_

            let prox_out_ = Raster $ step_dir </> "prox_out.tif"
            prox_out <- rasterProximity reproj_out prox_out_

            path <$> cropRasterWithBorder border_output_file prox_out (Raster out)
        )

vectorProximityFromUnion :: Vector -> Raster -> IO Raster
vectorProximityFromUnion union_ prox_out = do
  -- reproject to meters
  let reproj_out = Vector $ appendFilename "_reproj" $ path union_
  union <- reprojectVector union_ reproj_out "EPSG:3857"

  let dummy_out_ = Vector $ appendFilename "_with_dummy" $ path union
  dummy_out <- addDummyField union dummy_out_

  let rast_out_ = Raster $ appendFilename "_rast" $ path dummy_out
  rast_out <- rasterizePower dummy_out rast_out_

  rasterProximity rast_out prox_out

vectorProximityFromFiles :: String -> Vector -> [Vector] -> Raster -> IO Raster
vectorProximityFromFiles config_name b@(Vector border) is (Raster out) = do
  border_buff <- bufferBorder "out" b
  guardFileF Raster out $
    void $
      stepWrapper RemoveStepDir config_name "vectorProximityFromFiles"
        ( \step_dir -> do
            let union_out = Vector $ step_dir </> "step_union.shp"
            print $ path border_buff
            union_out_vec <- cropThenUnionVectors config_name border_buff is union_out
            let prox_out_ = Raster $ step_dir </> "prox_out.tif"
            prox_out <- vectorProximityFromUnion union_out_vec prox_out_
            path <$> cropRasterWithBorder (Vector border) prox_out (Raster out)
        )

cropThenUnionVectors :: String -> Vector -> [Vector] -> Vector -> IO Vector
cropThenUnionVectors config_name border is o@(Vector out) = do
  guardFileF Vector out $
    void $
      stepWrapper RemoveStepDir config_name "cropThenUnionVectors"
        ( \step_dir -> do
            let os = Vector <$> sequentialFilenames step_dir is ".shp"
            zipWithM_ (cropVectorWithBorder border) is os
            unionAllVectors config_name os o
        )

cropThenAverageRasters :: String -> Vector -> [Raster] -> Raster -> IO Raster
cropThenAverageRasters config_name border is o@(Raster out) = do
  guardFileF Raster out $
    void $
      stepWrapper RemoveStepDir config_name "cropThenAverageRasters"
        ( \step_dir -> do
            let os = Raster <$> sequentialFilenames step_dir is ".tif"
            zipWithM_ (cropRasterWithBorder border) is os
            averageRastersQGIS [] os o
        )

cropThenUnionRasters :: String -> Vector -> [Raster] -> Raster -> IO Raster
cropThenUnionRasters config_name border is o@(Raster out) = do
  guardFileF Raster out $
    void $
      stepWrapper RemoveStepDir config_name "cropThenUnionRasters"
        ( \step_dir -> do
            let os = Raster <$> sequentialFilenames step_dir is ".tif"
            zipWithM_ (cropRasterWithBorderExtents border) is os
            unionRasters os o
        )
