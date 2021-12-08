module Preprocessing.Combined where

import Control.Monad (void, zipWithM_)
import Preprocessing.Core (
  addDummyField,
  averageRaster,
  cropRasterWithBorder,
  cropRasterWithBorderExtents,
  cropVectorWithBorder,
  dissolveVector,
  filterVectorByField,
  rasterProximity,
  rasterizePower,
  removeFields,
  reprojectVector,
  stepWrapper,
  unionRasters,
  unionVectors,
  vectorizeLandUse, bufferVector, aspectFromElevation, slopeFromElevation, vectorize, extractVectorAttribute, vectorDifference, bufferBorder
 )
import System.Directory (removePathForcibly, renameFile)
import Utils (
  ShouldRemoveStepDir (DontRemoveStepDir, RemoveStepDir),
  appendFilename,
  foldM1,
  guardFile,
  sequentialFilenames, Constraint
 )
import Analysis (standardize, standardizeQGIS)
import System.FilePath ((</>), replaceExtension, takeDirectory, takeBaseName)
import Core (Direction(LessBetter, MoreBetter), ConstraintData (c_direction, distance), AspectData (limit1, limit2), rasterCalculator, finalRasterCalculator)

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
             -- TODO: do this in the `cropVectorWithBorder` function instead?
             let no_fields_i1_ = appendFilename "_no_fields" i1
             i1_no_fields <- removeFields i1 no_fields_i1_

             let no_fields_i2_ = appendFilename "_no_fields" i2
             i2_no_fields <- removeFields i2 no_fields_i2_

             let step_output = step_dir <> "/step_" <> show this_idx <> ".shp"
             res <- unionVectors i1_no_fields i2_no_fields step_output
             pure (next_idx, res)
        )

residentialProximity :: String -> [String] -> FilePath -> IO FilePath
residentialProximity border_output_file [land_use_in, land_use_field, residential_field_value] out = do
  let out_dir = takeDirectory border_output_file
  border_buff <- bufferBorder out_dir border_output_file
  guardFile out $
    void $ stepWrapper DontRemoveStepDir "residentialProximity"
      (\step_dir -> do
          let land_use_out_ = step_dir <> "/land_use_out.tif"
          land_use_out <-
            cropRasterWithBorder border_buff land_use_in land_use_out_

          let vect_land_use_out_ = step_dir <> "/land use poly.shp"
          vect_land_use_out <- vectorizeLandUse land_use_out vect_land_use_out_

          let reproj_land_use_out_ = step_dir <> "/land_use_reproj.shp"
          reproj_land_use_out <- reprojectLandUse vect_land_use_out reproj_land_use_out_

          let residential_out_vec_ = step_dir <> "/residential_vec.shp"
          residential_out_vec <-
            filterVectorByField
              land_use_field
              residential_field_value
              reproj_land_use_out
              residential_out_vec_

          let prox_out_ = step_dir </> "prox_out.tif"
          prox_out <- vectorProximityFromUnion residential_out_vec prox_out_

          cropRasterWithBorder border_output_file prox_out out
      )
  pure out

residentialProximityNew :: String -> [String] -> FilePath -> IO FilePath
residentialProximityNew border_output_file [land_use_in] out = do
  let out_dir = takeDirectory border_output_file
  border_buff <- bufferBorder out_dir border_output_file
  guardFile out $
    void $ stepWrapper DontRemoveStepDir "residentialProximity"
      (\step_dir -> do
          let land_use_out_ = step_dir <> "/land_use_out.tif"
          land_use_out <-
            cropRasterWithBorder border_buff land_use_in land_use_out_

          let residential_out_ = step_dir </> "residential_only.tif"
          let expr =
                "0*logical_or(logical_or(A!=22, A!=23), A!=24) + 1*logical_or(logical_or(A==22, A==23), A==24)"
          residential_out <- standardize expr land_use_out residential_out_

          let prox_out_ = step_dir </> "prox_out.tif"
          prox_out <- rasterProximity residential_out prox_out_

          cropRasterWithBorder border_output_file prox_out out
      )
  pure out

residentialConstraint :: ConstraintData -> String -> [String] -> FilePath -> IO FilePath
residentialConstraint cons border [land_use_in] out = do
  guardFile out $
    void $ stepWrapper DontRemoveStepDir "residentialConstraint"
      (\step_dir -> do
          -- The or operator doesn't seem to be working, so this is a hack
          let num_file x = step_dir </> (x <> ".tif")
          let expr num i = i <> "!= " <> num
          out1 <- standardizeQGIS (expr "22") land_use_in $ num_file "22"
          out2 <- standardizeQGIS (expr "23") land_use_in $ num_file "23"
          out3 <- standardizeQGIS (expr "24") land_use_in $ num_file "24"

          case c_direction cons of
            MoreBetter -> finalRasterCalculator [out1, out2, out3] out
            LessBetter -> do
              let tmp_ = step_dir </> "tmp.tif"
              tmp <- finalRasterCalculator [out1, out2, out3] tmp_
              standardize "0*(A==1)+1*(A==0)" tmp out
      )
  pure out

reprojectLandUse :: String -> String -> IO String
reprojectLandUse vect_land_use_out reproj_land_use_out =
  reprojectVector
    reproj_land_use_out
    vect_land_use_out
    "EPSG:4326"

vectorProximityFromUnion :: String -> String -> IO String
vectorProximityFromUnion union_ prox_out = do
  let reproj_out = appendFilename "_reproj" union_
  union <- reprojectVector union_ reproj_out "EPSG:3857"

  let dummy_out_ = appendFilename "_with_dummy" union
  dummy_out <- addDummyField union dummy_out_

  let rast_out_ = appendFilename "_rast" dummy_out
  rast_out <- rasterizePower dummy_out rast_out_

  rasterProximity rast_out prox_out

-- | The difference between this and vectorProximityFromUnion
-- is that this one must have a buffer for lines; it doesn't make sense
-- to constraint out a line of zero width
-- Also the rasterization accounts for the direction: if this was a factor
-- that would be done in the standardization step, but this is a constraint
-- and it's better to do everything related to constraints in a single step
vectorConstraint :: ConstraintData -> String -> String -> IO String
vectorConstraint cons union_ out = do
  let reproj_out = appendFilename "_reproj" union_
  union <- reprojectVector union_ reproj_out "EPSG:3857"

  let buf_out_ = appendFilename "_buffered" union
  buf_out <- bufferVector (distance cons) union buf_out_

  let dummy_out_ = appendFilename "_with_dummy" buf_out
  dummy_out <- addDummyField buf_out dummy_out_

  case c_direction cons of
    LessBetter -> rasterizePower dummy_out out
    MoreBetter -> do
      let rast_out_ = appendFilename "_rasterized" buf_out
      rast_out <- rasterizePower dummy_out rast_out_
      -- Need to invert the raster
      standardize "0*(A==1)+1*(A==0)" rast_out out

vectorProximityFromFiles :: String -> [String] -> String -> IO String
vectorProximityFromFiles border is out = do
  let out_dir = takeDirectory border
  border_buff <- bufferBorder out_dir border
  guardFile out $
    void $ stepWrapper RemoveStepDir "vectorProximityFromFiles"
      (\step_dir -> do
          let union_out = step_dir <> "/step_union.shp"
          print border_buff
          union_out_vec <- cropThenUnionVectors border_buff is union_out
          let prox_out_ = step_dir </> "prox_out.tif"
          prox_out <- vectorProximityFromUnion union_out_vec prox_out_
          cropRasterWithBorder border prox_out out
      )
  pure out

vectorConstraintFromFiles :: ConstraintData -> String -> [String] -> FilePath -> IO FilePath
vectorConstraintFromFiles cons border is prox_out = do
  guardFile prox_out $
    void $ stepWrapper RemoveStepDir "vectorConstraintFromFiles"
      (\step_dir -> do
          let union_out = step_dir <> "/step_union.shp"
          union_out_vec <- cropThenUnionVectors border is union_out
          vectorConstraint cons union_out_vec prox_out
      )
  pure prox_out

cropThenUnionVectors :: String -> [String] -> String -> IO String
cropThenUnionVectors border is out = do
  guardFile out $
    void $ stepWrapper RemoveStepDir "cropThenUnionVectors"
      (\step_dir -> do
          let os = sequentialFilenames step_dir is ".shp"
          zipWithM_ (cropVectorWithBorder border) is os
          unionAllVectors os out
      )
  pure out

cropThenAverageRasters :: String -> [String] -> String -> IO String
cropThenAverageRasters border is out = do
  guardFile out $
    void $ stepWrapper RemoveStepDir "cropThenAverageRasters"
      (\step_dir -> do
          let os = sequentialFilenames step_dir is ".tif"
          zipWithM_ (cropRasterWithBorder border) is os
          averageRaster os out
      )
  pure out

cropThenUnionRasters :: String -> [String] -> String -> IO String
cropThenUnionRasters border is out = do
  guardFile out $
    void $ stepWrapper RemoveStepDir "cropThenUnionRasters"
      (\step_dir -> do
          let os = sequentialFilenames step_dir is ".tif"
          zipWithM_ (cropRasterWithBorderExtents border) is os
          unionRasters os out
      )
  pure out

elevationConstraint :: ConstraintData -> String -> [String] -> FilePath -> IO FilePath
elevationConstraint cons border ins out = do
  guardFile out $
    void $ stepWrapper RemoveStepDir "elevationConstraint"
      (\step_dir -> do
          let elevation_out = step_dir </> "elevation.tif"
          elevation <- cropThenUnionRasters border ins elevation_out
          let dist = show $ distance cons
          let expr_ =
                case c_direction cons of
                  LessBetter -> "0*(A>" <> dist <> ") + 1*(A<=" <> dist <> ")"
                  MoreBetter -> "0*(A<" <> dist <> ") + 1*(A>=" <> dist <> ")"

          standardize expr_ elevation out
      )
  pure out

aspectConstraint :: AspectData -> a -> [String] -> FilePath -> IO FilePath
aspectConstraint cons b ins out = do
  guardFile out $
    void $ stepWrapper RemoveStepDir "aspectConstraint"
      (\step_dir -> do
          let aspect_out = step_dir </> "aspect.tif"
          aspect <- aspectFromElevation b ins aspect_out
          let l1 = show $ limit1 cons
          let l2 = show $ limit2 cons
          let expr_ =
                "0*(logical_or(A<"
                <> l1
                <> ", A>"
                <> l2
                <> "))+1*(logical_and(A>"
                <> l1
                <> ", A<"
                <> l2
                <> "))"
          standardize expr_ aspect out
      )
  pure out

slopeConstraint :: ConstraintData -> a -> [String] -> FilePath -> IO FilePath
slopeConstraint cons b ins out = do
  guardFile out $
    void $ stepWrapper RemoveStepDir "slopeConstraint"
      (\step_dir -> do
          let slope_out = step_dir </> "slope.tif"
          slope <- slopeFromElevation b ins slope_out
          let dist = show $ distance cons
          let expr_ =
                case c_direction cons of
                  LessBetter -> "0*(A>" <> dist <> ") + 1*(A<=" <> dist <> ")"
                  MoreBetter -> "0*(A<" <> dist <> ") + 1*(A>=" <> dist <> ")"
          standardize expr_ slope out
      )
  pure out
