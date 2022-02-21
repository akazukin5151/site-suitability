module Preprocessing.Constraints where

import Analysis (standardize, standardizeQGIS)
import Control.Monad (void)
import Core (AspectData (limit1, limit2), ConstraintData (c_direction, distance), Direction (LessBetter, MoreBetter), Layer (path), Raster (Raster), Vector (Vector), finalRasterCalculator)
import Preprocessing.Combined (cropThenUnionRasters, cropThenUnionVectors)
import Preprocessing.Core (stepWrapper)
import Preprocessing.Core.Raster (aspectFromElevation, slopeFromElevation, unionRastersIfMultiple, reprojectRaster, rasterProximity)
import Preprocessing.Core.Vector (
  addDummyField,
  bufferVector,
  rasterizeVector,
  reprojectVector
 )
import System.FilePath ((</>))
import Utils (
  ShouldRemoveStepDir (DontRemoveStepDir, RemoveStepDir),
  appendFilename,
  guardFileF,
 )

residentialConstraint :: ConstraintData -> String -> a -> [Raster] -> Raster -> IO Raster
residentialConstraint cons config_name _ is o@(Raster out) = do
  guardFileF Raster out $
    void $
      stepWrapper DontRemoveStepDir config_name "residentialConstraint"
        ( \step_dir -> do
            -- The or operator doesn't seem to be working, so this is a hack
            land_use_in <- unionRastersIfMultiple pure step_dir is

            case distance cons of
              0 -> do
                let num_file x = Raster $ step_dir </> (x <> ".tif")
                let expr num i = i <> "!= " <> num
                out1 <- standardizeQGIS (expr "22") land_use_in $ num_file "22"
                out2 <- standardizeQGIS (expr "23") land_use_in $ num_file "23"
                out3 <- standardizeQGIS (expr "24") land_use_in $ num_file "24"
                case c_direction cons of
                  MoreBetter -> finalRasterCalculator [out1, out2, out3] o
                  LessBetter -> do
                    let tmp_ = Raster $ step_dir </> "tmp.tif"
                    tmp <- finalRasterCalculator [out1, out2, out3] tmp_
                    standardize "0*(A==1)+1*(A==0)" tmp o
              d -> do
                -- Copied from residentialProximity
                -- reproject to meters
                let residential_out_ = Raster $ step_dir </> "residential_only.tif"
                let expr =
                      "0*logical_or(logical_or(A!=22, A!=23), A!=24) + 1*logical_or(logical_or(A==22, A==23), A==24)"
                residential_out <- standardize expr land_use_in residential_out_

                let reproj_out_ = Raster $ step_dir </> "residential_reproj.tif"
                reproj_out <- reprojectRaster residential_out reproj_out_

                let prox_out_ = Raster $ step_dir </> "prox_out.tif"
                prox_out <- rasterProximity reproj_out prox_out_

                let expr =
                      case c_direction cons of
                        MoreBetter -> "1*(A>=" <> show d <> ")+0*(A<" <> show d <> ")"
                        LessBetter -> "0*(A>=" <> show d <> ")+1*(A<" <> show d <> ")"
                standardize expr prox_out o
        )

abstractConstraint :: (Raster -> IO Raster)
                   -> String
                   -> String
                   -> String
                   -> Raster
                   -> IO Raster
abstractConstraint func config_name name expr o@(Raster out) = do
  guardFileF Raster out $
    void $
      stepWrapper RemoveStepDir config_name (name <> "Constraint")
        ( \step_dir -> do
            let constraint_out_ = Raster $ step_dir </> (name <> ".tif")
            constraint_out <- func constraint_out_
            standardize expr constraint_out o
        )

elevationConstraint :: ConstraintData -> String -> Vector -> [Raster] -> Raster -> IO Raster
elevationConstraint cons config_name border ins =
  abstractConstraint func "elevation" config_name expr_
  where
    dist = show $ distance cons
    expr_ =
      case c_direction cons of
        LessBetter -> "0*(A>" <> dist <> ") + 1*(A<=" <> dist <> ")"
        MoreBetter -> "0*(A<" <> dist <> ") + 1*(A>=" <> dist <> ")"
    func elevation_out =
      cropThenUnionRasters config_name border ins elevation_out

aspectConstraint :: AspectData -> String -> Vector -> [Raster] -> Raster -> IO Raster
aspectConstraint cons config_name b ins =
  abstractConstraint func "aspect" config_name expr_
  where
    l1 = show $ limit1 cons
    l2 = show $ limit2 cons
    -- 0 if outside bounds, 1 if within bounds
    or_expr = "logical_or(A<" <> l1 <> ", A>" <> l2 <> ")"
    and_expr = "logical_and(A>" <> l1 <> ", A<" <> l2 <> ")"
    expr_ = "0*(" <> or_expr <> ")+1*(" <> and_expr <> ")"
    func aspect_out =
      aspectFromElevation config_name b ins aspect_out

slopeConstraint :: ConstraintData -> String -> Vector -> [Raster] -> Raster -> IO Raster
slopeConstraint cons config_name b ins = abstractConstraint func "slope" config_name expr_
  where
    dist = show $ distance cons
    expr_ =
      case c_direction cons of
        LessBetter -> "0*(A>" <> dist <> ") + 1*(A<=" <> dist <> ")"
        MoreBetter -> "0*(A<" <> dist <> ") + 1*(A>=" <> dist <> ")"
    func slope_out =
      slopeFromElevation config_name b ins slope_out

{- | The difference between this and vectorProximityFromUnion
 is that this one must have a buffer for lines; it doesn't make sense
 to constraint out a line of zero width
 Also the rasterization accounts for the direction: if this was a factor
 that would be done in the standardization step, but this is a constraint
 and it's better to do everything related to constraints in a single step
-}
vectorConstraint :: ConstraintData -> Vector -> Raster -> IO Raster
vectorConstraint cons union_ out = do
  let reproj_out = Vector $ appendFilename "_reproj" $ path union_
  union <- reprojectVector union_ reproj_out "EPSG:3857"

  let buf_out_ = Vector $ appendFilename "_buffered" $ path union
  buf_out <- bufferVector (distance cons) union buf_out_

  let dummy_out_ = Vector $ appendFilename "_with_dummy" $ path buf_out
  dummy_out <- addDummyField buf_out dummy_out_

  case c_direction cons of
    LessBetter -> rasterizeVector dummy_out out
    MoreBetter -> do
      let rast_out_ = Raster $ appendFilename "_rasterized" $ path buf_out
      rast_out <- rasterizeVector dummy_out rast_out_
      -- Need to invert the raster
      standardize "0*(A==1)+1*(A==0)" rast_out out

vectorConstraintFromFiles :: ConstraintData -> String -> Vector -> [Vector] -> Raster -> IO Raster
vectorConstraintFromFiles cons config_name border is o@(Raster prox_out) = do
  guardFileF Raster prox_out $
    void $
      stepWrapper RemoveStepDir config_name "vectorConstraintFromFiles"
        ( \step_dir -> do
            let union_out = Vector $ step_dir </> "step_union.shp"
            union_out_vec <- cropThenUnionVectors config_name border is union_out
            vectorConstraint cons union_out_vec o
        )
