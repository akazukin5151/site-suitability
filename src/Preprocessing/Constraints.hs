module Preprocessing.Constraints where

import Core (ConstraintData (distance, c_direction), Direction (MoreBetter, LessBetter), AspectData (limit1, limit2), finalRasterCalculator)
import Utils
    ( ShouldRemoveStepDir(RemoveStepDir, DontRemoveStepDir),
      guardFile', appendFilename )
import Control.Monad (void)
import Preprocessing.Core (stepWrapper)
import Preprocessing.Core.Raster (aspectFromElevation, slopeFromElevation)
import System.FilePath ((</>))
import Analysis (standardize, standardizeQGIS)
import Preprocessing.Combined (cropThenUnionRasters, cropThenUnionVectors)
import Preprocessing.Core.Vector
    ( addDummyField, bufferVector, rasterizePower, reprojectVector )

residentialConstraint :: ConstraintData -> String -> [String] -> FilePath -> IO FilePath
residentialConstraint cons border [land_use_in] out = do
  guardFile' out $
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

elevationConstraint :: ConstraintData -> String -> [String] -> FilePath -> IO FilePath
elevationConstraint cons border ins out = do
  guardFile' out $
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

aspectConstraint :: AspectData -> a -> [String] -> FilePath -> IO FilePath
aspectConstraint cons b ins out = do
  guardFile' out $
    void $ stepWrapper RemoveStepDir "aspectConstraint"
      (\step_dir -> do
          let aspect_out = step_dir </> "aspect.tif"
          aspect <- aspectFromElevation b ins aspect_out
          let l1 = show $ limit1 cons
          let l2 = show $ limit2 cons
          -- 0 if outside bounds, 1 if within bounds
          let or_expr = "logical_or(A<" <> l1 <> ", A>" <> l2 <> ")"
          let and_expr = "logical_and(A>" <> l1 <> ", A<" <> l2 <> ")"
          let expr_ = "0*(" <> or_expr <> ")+1*(" <> and_expr <> ")"
          standardize expr_ aspect out
      )

slopeConstraint :: ConstraintData -> a -> [String] -> FilePath -> IO FilePath
slopeConstraint cons b ins out = do
  guardFile' out $
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

vectorConstraintFromFiles :: ConstraintData -> String -> [String] -> FilePath -> IO FilePath
vectorConstraintFromFiles cons border is prox_out = do
  guardFile' prox_out $
    void $ stepWrapper RemoveStepDir "vectorConstraintFromFiles"
      (\step_dir -> do
          let union_out = step_dir <> "/step_union.shp"
          union_out_vec <- cropThenUnionVectors border is union_out
          vectorConstraint cons union_out_vec prox_out
      )
