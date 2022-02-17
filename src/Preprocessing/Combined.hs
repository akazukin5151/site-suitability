module Preprocessing.Combined where

import Control.Monad (void, zipWithM_)
import Preprocessing.Core ( stepWrapper )
import Preprocessing.Core.Raster
    ( averageRaster,
      cropRasterWithBorder,
      cropRasterWithBorderExtents,
      rasterProximity,
      unionRasters, reprojectRaster )
import Preprocessing.Core.Vector
    ( addDummyField,
      bufferBorder,
      cropVectorWithBorder,
      dissolveVector,
      rasterizePower,
      removeFields,
      reprojectVector,
      unionVectors )
import System.Directory (removePathForcibly)
import Utils (
  ShouldRemoveStepDir (DontRemoveStepDir, RemoveStepDir),
  appendFilename,
  foldM1,
  sequentialFilenames, guardFile'
 )
import Analysis (standardize)
import System.FilePath ((</>), takeDirectory)

-- | The output name must not have an extension; it is not a filepath or vector
unionAllVectors :: [String] -> String -> IO String
unionAllVectors is out = do
  -- Remove all fields first
  (_, final_vec) <- foldM1 unionStep $ zip [0 ..] is
  res <- dissolveVector final_vec out
  removePathForcibly "internal_steps_unionStep"
  pure res
    where
      -- | Union two vectors with the wrapper (puts the result in another dir)
      unionStep :: (Int, String) -> (Int, String) -> IO (Int, String)
      unionStep (this_idx, i1) (next_idx, i2) =
        stepWrapper DontRemoveStepDir "unionStep"
        (\step_dir -> do
             let no_fields_i1_ = appendFilename "_no_fields" i1
             i1_no_fields <- removeFields i1 no_fields_i1_

             let no_fields_i2_ = appendFilename "_no_fields" i2
             i2_no_fields <- removeFields i2 no_fields_i2_

             let step_output = step_dir </> ("step_" <> show this_idx <> ".shp")
             res <- unionVectors i1_no_fields i2_no_fields step_output
             pure (next_idx, res)
        )

residentialProximity :: String -> [String] -> FilePath -> IO FilePath
residentialProximity border_output_file [land_use_in] out = do
  let out_dir = takeDirectory border_output_file
  border_buff <- bufferBorder out_dir border_output_file
  guardFile' out $
    void $ stepWrapper RemoveStepDir "residentialProximity"
      (\step_dir -> do
          let land_use_out_ = step_dir </> "land_use_out.tif"
          land_use_out <-
            cropRasterWithBorder border_buff land_use_in land_use_out_

          let residential_out_ = step_dir </> "residential_only.tif"
          let expr =
                "0*logical_or(logical_or(A!=22, A!=23), A!=24) + 1*logical_or(logical_or(A==22, A==23), A==24)"
          residential_out <- standardize expr land_use_out residential_out_

          -- reproject to meters
          let reproj_out_ = step_dir </> "residential_reproj.tif"
          reproj_out <- reprojectRaster residential_out reproj_out_

          let prox_out_ = step_dir </> "prox_out.tif"
          prox_out <- rasterProximity reproj_out prox_out_

          cropRasterWithBorder border_output_file prox_out out
      )

vectorProximityFromUnion :: String -> String -> IO String
vectorProximityFromUnion union_ prox_out = do
  -- reproject to meters
  let reproj_out = appendFilename "_reproj" union_
  union <- reprojectVector union_ reproj_out "EPSG:3857"

  let dummy_out_ = appendFilename "_with_dummy" union
  dummy_out <- addDummyField union dummy_out_

  let rast_out_ = appendFilename "_rast" dummy_out
  rast_out <- rasterizePower dummy_out rast_out_

  rasterProximity rast_out prox_out

vectorProximityFromFiles :: String -> [String] -> String -> IO String
vectorProximityFromFiles border is out = do
  let out_dir = takeDirectory border
  border_buff <- bufferBorder out_dir border
  guardFile' out $
    void $ stepWrapper RemoveStepDir "vectorProximityFromFiles"
      (\step_dir -> do
          let union_out = step_dir </> "step_union.shp"
          print border_buff
          union_out_vec <- cropThenUnionVectors border_buff is union_out
          let prox_out_ = step_dir </> "prox_out.tif"
          prox_out <- vectorProximityFromUnion union_out_vec prox_out_
          cropRasterWithBorder border prox_out out
      )

cropThenUnionVectors :: String -> [String] -> String -> IO String
cropThenUnionVectors border is out = do
  guardFile' out $
    void $ stepWrapper RemoveStepDir "cropThenUnionVectors"
      (\step_dir -> do
          let os = sequentialFilenames step_dir is ".shp"
          zipWithM_ (cropVectorWithBorder border) is os
          unionAllVectors os out
      )

cropThenAverageRasters :: String -> [String] -> String -> IO String
cropThenAverageRasters border is out = do
  guardFile' out $
    void $ stepWrapper RemoveStepDir "cropThenAverageRasters"
      (\step_dir -> do
          let os = sequentialFilenames step_dir is ".tif"
          zipWithM_ (cropRasterWithBorder border) is os
          averageRaster os out
      )

cropThenUnionRasters :: String -> [String] -> String -> IO String
cropThenUnionRasters border is out = do
  guardFile' out $
    void $ stepWrapper RemoveStepDir "cropThenUnionRasters"
      (\step_dir -> do
          let os = sequentialFilenames step_dir is ".tif"
          zipWithM_ (cropRasterWithBorderExtents border) is os
          unionRasters os out
      )
