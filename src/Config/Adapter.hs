module Config.Adapter where

import Analysis (gaussian, rangeStandardize, reverseRangeStandardize, standardize, standardizeQGIS, suhSigmoid)
import Control.Arrow ((<<<))
import Core (AspectData, ConstraintData, Layer (path), Raster (Raster), Vector (Vector))
import Preprocessing.Combined (
  cropThenAverageRasters,
  cropThenUnionRasters,
  residentialProximity,
  vectorProximityFromFiles,
 )
import Preprocessing.Constraints (
  aspectConstraint,
  elevationConstraint,
  residentialConstraint,
  slopeConstraint,
  vectorConstraintFromFiles,
 )
import Preprocessing.Core.Raster (
  aspectFromElevation,
  slopeFromElevation,
 )

type CriteriaFunc = String -> String -> [String] -> String -> IO String

criteriaAdapter :: (String -> Vector -> [Raster] -> Raster -> IO Raster) -> CriteriaFunc
criteriaAdapter f cn border is out =
  path <$> f cn (Vector border) (Raster <$> is) (Raster out)

cropThenAverageRasters' :: CriteriaFunc
cropThenAverageRasters' = criteriaAdapter cropThenAverageRasters

cropThenUnionRasters' :: CriteriaFunc
cropThenUnionRasters' = criteriaAdapter cropThenUnionRasters

slopeFromElevation' :: CriteriaFunc
slopeFromElevation' = criteriaAdapter slopeFromElevation

aspectFromElevation' :: CriteriaFunc
aspectFromElevation' = criteriaAdapter aspectFromElevation

residentialProximity' :: CriteriaFunc
residentialProximity' = criteriaAdapter residentialProximity

vectorProximityFromFiles' :: CriteriaFunc
vectorProximityFromFiles' config_name border is out =
  path <$> vectorProximityFromFiles config_name (Vector border) (Vector <$> is) (Raster out)

type ConstraintFunc a = a -> String -> String -> [String] -> String -> IO String

constraintAdapter :: (a -> String -> Vector -> [Raster] -> Raster -> IO Raster)
                  -> ConstraintFunc a
constraintAdapter f cons config_name  b is out =
  path <$> f cons config_name (Vector b) (Raster <$> is) (Raster out)

residentialConstraint' :: ConstraintFunc ConstraintData
residentialConstraint' = constraintAdapter residentialConstraint

vectorConstraintFromFiles' :: ConstraintFunc ConstraintData
vectorConstraintFromFiles' cons config_name b is out =
  path <$> vectorConstraintFromFiles cons config_name (Vector b) (Vector <$> is) (Raster out)

elevationConstraint' :: ConstraintFunc ConstraintData
elevationConstraint' = constraintAdapter elevationConstraint

aspectConstraint' :: ConstraintFunc AspectData
aspectConstraint' = constraintAdapter aspectConstraint

slopeConstraint' :: ConstraintFunc ConstraintData
slopeConstraint' = constraintAdapter slopeConstraint

type StdFunc = String -> String -> IO String

stdAdapter :: (Raster -> Raster -> IO Raster) -> StdFunc
stdAdapter f a b = path <$> f (Raster a) (Raster b)

rangeStandardize' :: StdFunc
rangeStandardize' = stdAdapter rangeStandardize

reverseRangeStandardize' :: StdFunc
reverseRangeStandardize' = stdAdapter reverseRangeStandardize

standardize' :: String -> StdFunc
standardize' s = stdAdapter (standardize s)

suhSigmoid' :: (a -> Double)
            -> (a -> Double)
            -> (a -> Maybe Double)
            -> a
            -> StdFunc
suhSigmoid' midpoint spread divide d =
  stdAdapter (standardizeQGIS (suhSigmoid (midpoint d) (spread d) (divide d) <<< Raster))

standardizeGaussian :: Double -> Double -> Double -> StdFunc
standardizeGaussian b c div' =
  stdAdapter (standardizeQGIS (gaussian b c div' <<< Raster))
