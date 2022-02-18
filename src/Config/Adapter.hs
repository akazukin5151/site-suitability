module Config.Adapter where

import Preprocessing.Combined (
  cropThenAverageRasters,
  cropThenUnionRasters,
  vectorProximityFromFiles, residentialProximity
 )
import Preprocessing.Core.Raster (
  aspectFromElevation,
  slopeFromElevation,
 )
import Preprocessing.Constraints (
  vectorConstraintFromFiles, residentialConstraint, elevationConstraint, aspectConstraint, slopeConstraint
 )
import Analysis (rangeStandardize, reverseRangeStandardize, standardize, suhSigmoid, gaussian, standardizeQGIS)
import Core (Path(path), Vector (Vector), Raster (Raster), AspectData, ConstraintData)
import Control.Arrow ((<<<))

type CriteriaFunc = String -> [String] -> String -> IO String

criteriaAdapter :: (Vector -> [Raster] -> Raster -> IO Raster)
                -> CriteriaFunc
criteriaAdapter f border is out =
  path <$> f (Vector border) (Raster <$> is) (Raster out)

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
vectorProximityFromFiles' border is out =
  path <$> vectorProximityFromFiles (Vector border) (Vector <$> is) (Raster out)


type ConstraintFunc a = a -> String -> [String] -> String -> IO String

constraintAdapter :: (a -> Vector -> [Raster] -> Raster -> IO Raster)
                  -> ConstraintFunc a
constraintAdapter f cons b is out =
  path <$> f cons (Vector b) (Raster <$> is) (Raster out)

residentialConstraint' :: ConstraintFunc ConstraintData
residentialConstraint' = constraintAdapter residentialConstraint

vectorConstraintFromFiles' :: ConstraintFunc ConstraintData
vectorConstraintFromFiles' cons b is out =
  path <$> vectorConstraintFromFiles cons (Vector b) (Vector <$> is) (Raster out)

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
