-- | Things for after preprocessing - standardization and weighting
{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import Core (finalRasterCalculator)
import Utils (
  quoteDouble,
  quoteSingle,
  runCmd, guardFile'
 )


import Data.Scientific (Scientific)
import System.Directory (doesFileExist)
import Control.Monad (when)
import Metadata (getMinMax)

{- | Broadcast/map `calc_expr` function to every cell in `input_file`
 | calc_expr examples:
 |     - `log10(A)`
 |     - `constant * logical_and(...)`
 |     - `log10(A) * logical_and(...)`
 | See also:
 |     - https://gdal.org/programs/gdal_calc.html
 |     - https://feed.terramonitor.com/normalizing-data-with-gdal_calc-py/
 |     - native:fuzzifyrasterlargemembership
-}
standardize :: String -> String -> String -> IO String
standardize calc_expr i out = do
  guardFile' out $
    runCmd
      "gdal_calc.py"
      [ "-A"
      , quoteDouble i
      , "--outfile"
      , quoteDouble out
      , "--calc=" <> quoteDouble calc_expr
      ]

standardizeQGIS :: (String -> String) -> String -> FilePath -> IO FilePath
standardizeQGIS calc_expr i out = do
  guardFile' out $
    runCmd "qgis_process"
      [ "run"
      , "qgis:rastercalculator"
      , "--"
      , "CELLSIZE=0"
      --, "CRS='EPSG:4326'"
      , "EXPRESSION=" <> quoteSingle (calc_expr $ quoteDouble $ i <> "@1")
      -- extents seem to be optional
      , "OUTPUT=" <> quoteSingle out
      , "LAYERS=" <> quoteDouble i
      ]

abstractRangeStandardize :: (Scientific -> Scientific -> String)
                         -> FilePath -> FilePath -> IO FilePath
abstractRangeStandardize calc_expr i out = do
  guardFile' out $ do
    (min', max') <- getMinMax i
    print (min', max')
    when (min' == 0 && max' == 0) $
      error "both min and max are equal to 0"
    when (min' == max') $
      error "both min and max are equal"

    -- readCmd won't work, but if this fails try `conda deactivate`
    runCmd
      "qgis_process"
      [ "run"
      , "gdal:rastercalculator"
      , "--"
      , "INPUT_A=" <> quoteSingle i
      , "BAND_A=1" -- hope this is a constant...
      , "FORMULA=" <> quoteSingle (calc_expr min' max')
      , "OUTPUT=" <> quoteSingle out
      , "RTYPE=5" -- float32
      ]
    -- gdal calc is buggy for some reason
    -- happens for insolation and elevation; doesn't seem to happen for other cases
    --void $ standardize calc_expr in_ out

-- | Higher values are better
-- This is an alternative in case if rangeStandardize hangs
-- but seems like it successfully calculates the result but fails to terminate
-- if the output file is correct then killing `qgis_process` and continuing
-- would suffice, and there would be no need to use this function
rangeStandardize' :: String -> String -> IO String
rangeStandardize' i o = do
  guardFile' o $ do
    (min', max') <- getMinMax i
    _ <- standardize (calc_expr min' max') i o
    pure ()
  where
    upper min'          = "(A -" <> show min' <> ") "
    lower max' min'     = "(" <> show max' <> " - " <> show min' <> ")"
    calc_expr min' max' = upper min' <> " / " <> lower max' min'

-- | Higher values are better
rangeStandardize :: String -> String -> IO String
rangeStandardize = abstractRangeStandardize calc_expr
  where
    upper min'          = "(A -" <> show min' <> ") "
    lower max' min'     = "(" <> show max' <> " - " <> show min' <> ")"
    calc_expr min' max' = upper min' <> " / " <> lower max' min'

-- | Lower values are better
reverseRangeStandardize :: String -> String -> IO String
reverseRangeStandardize = abstractRangeStandardize calc_expr
  where
    upper min'          = "(A -" <> show min' <> ") "
    lower max' min'     = "(" <> show max' <> " - " <> show min' <> ")"
    calc_expr min' max' = "1 - (" <> upper min' <> " / " <> lower max' min' <> ")"

-- | Increasing if spread is negative, decreasing if spread is positive
suhSigmoid :: Double -> Double -> Maybe Double -> String -> String
suhSigmoid midpoint spread mdivide i =
  upper <> " / " <> lower
    where
      upper = "1"
      lower = "(1+" <> lower_frac_pow <> ")"
      lower_frac = "( " <> x_var <> "/" <> show midpoint <> ")"
      lower_frac_pow = "(" <> lower_frac <> "^(" <> show spread <> "))"
      x_var =
        case mdivide of
          Nothing -> i
          Just d -> "(" <> i <> " / (" <> show d <> "))"

linear :: Double -> Double -> String
linear gradient y_intercept =
  mx <> plus_c
    where
      mx = "(" <> show gradient <> ")*A"
      plus_c = "+(" <> show y_intercept <> ")"

linearClamped :: Double -> Double -> Double -> Double -> Double -> Double -> String
linearClamped lv rv gradient y_intercept clamp_left clamp_right =
  left_expr <> "+" <> linear_expr <> "+" <> right_expr
     where
       left_expr   = show lv <> "* (A<" <> show clamp_left  <> ")"
       right_expr  = show rv <> "* (A>" <> show clamp_right <> ")"
       linear_expr = "(" <> linear gradient y_intercept <> ") *" <> linear_cond
       linear_cond = "logical_and(" <> cond1 <> ", " <> cond2 <> ")"
       cond1       = "A>=" <> show clamp_left
       cond2       = "A<=" <> show clamp_right

linearClampedInc :: Double -> Double -> Double -> Double -> String
linearClampedInc = linearClamped 0 1

linearClampedDec :: Double -> Double -> Double -> Double -> String
linearClampedDec = linearClamped 1 0

gaussian :: Double -> Double -> Double -> String -> String
gaussian b c div' i =
  exp_ $ "(" <> upper <> " / " <> lower <> ")"
    where
      e = "2.71828182845904523536"
      exp_ x = e <> "^ (" <> x <> ")"
      upper = "- ((" <> upper_sub <> ") ^ 2)"
      upper_sub = x_var <> " - (" <> show b <> ")"
      x_var = "(" <> i <> "/" <> show div' <> ")"
      lower = "(2 * (" <> show c <> ")^2)"

weightCriteria :: Double -> String -> String -> IO String
weightCriteria w i o = do
  exists <- doesFileExist o
  if w == 1 && exists
    then pure o
    else standardize ("A*" <> show w) i o

-- | Output is the suitability score
multiplyAllCriteria :: [String] -> String -> IO String
multiplyAllCriteria = finalRasterCalculator
