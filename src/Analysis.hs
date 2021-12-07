-- TODO: move sub-expressions in equation strings to variables to make it clearer
{-# LANGUAGE OverloadedStrings #-}

module Analysis where

import Core (finalRasterCalculator)
import GHC.IO.Exception (ExitCode (ExitFailure))
import System.Exit (ExitCode (ExitSuccess))
import Utils (
  guardFile,
  quoteDouble,
  quoteSingle,
  readCmd,
  runCmd,
 )

import Data.Aeson (Value (Number, Object, String), decode)
import Data.ByteString.Lazy.UTF8 (fromString)

import Control.Lens ((^?))
import Data.Aeson.Lens (key, nth)
import Data.Scientific (Scientific)
import System.Directory (doesFileExist)
import Control.Monad (when)
import Data.Text (unpack)

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
  guardFile out $
    runCmd
      "gdal_calc.py"
      [ "-A"
      , quoteDouble i
      , "--outfile"
      , quoteDouble out
      , "--calc=" <> quoteDouble calc_expr
      ]
  pure out

standardizeQGIS :: (String -> String) -> String -> FilePath -> IO FilePath
standardizeQGIS calc_expr i out = do
  guardFile out $
    runCmd "qgis_process"
      [ "run"
      , "qgis:rastercalculator"
      , "--"
      , "CELLSIZE=0"
      --, "CRS='EPSG:4326'"
      , "EXPRESSION=" <> quoteSingle (calc_expr $ quoteDouble $ i <> "@1")
      -- TODO get extents; got it from insolation, which should be same as border
      --, "EXTENT='-114.808333333,-109.050000000,31.333333333,37.000000000'"
      , "OUTPUT=" <> quoteSingle out
      , "LAYERS=" <> quoteDouble i
      ]
  pure out

abstractRangeStandardize :: (Scientific -> Scientific -> String)
                         -> FilePath -> FilePath -> IO FilePath
abstractRangeStandardize calc_expr i out = do
  guardFile out $ do
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
  pure out

-- | Higher values are better
rangeStandardize' :: String -> String -> IO String
rangeStandardize' i o = do
  (min', max') <- getMinMax i
  standardize (calc_expr min' max') i o
  where
    calc_expr min' max' =
      "(A -" <> show min' <> ") / (" <> show max' <> " - " <> show min' <> ")"

-- | Higher values are better
rangeStandardize :: String -> String -> IO String
rangeStandardize = abstractRangeStandardize calc_expr
  where
    calc_expr min' max' =
      "(A -" <> show min' <> ") / (" <> show max' <> " - " <> show min' <> ")"

-- | Lower values are better
reverseRangeStandardize :: String -> String -> IO String
reverseRangeStandardize = abstractRangeStandardize calc_expr
  where
    calc_expr min' max' =
      "1 - ((A -" <> show min' <> ") / (" <> show max' <> " - " <> show min' <> "))"

getMinMax :: String -> IO (Scientific, Scientific)
getMinMax i = do
    (code, stdout, stderr) <- readCmd "gdalinfo" ["-mm", "-json", i]
    (min', max') <- case code of
      ExitFailure _ -> print stderr >> error "Failed to get min and max values"
      ExitSuccess -> case parseMinMax stdout of
                       Left x -> error x
                       Right x -> pure x
    pure (min', max')

parseMinMax :: String -> Either String (Scientific, Scientific)
parseMinMax stdout = do
  value <-
    case (decode (fromString stdout) :: Maybe Value) of
      Just x -> pure x
      _ -> Left "Failed to decode JSON"

  -- band contains an array, and `nth 0` takes the first item in the array
  fst_band <-
    case value ^? key "bands" . nth 0 of
      Just x -> pure x
      _ -> Left "Failed to get the first band"

  -- The metadata is more accurate than the computed
  -- metadata is a nested dict, eg:
  -- "metadata": { "": {"STATISTICS_MINIMUM": "1"}  }
  -- but if it fails, fall back to computedMin and computedMax
  case fst_band ^? key "metadata" . key "" of
    Just metadata -> do
      mmin <-
        case metadata ^? key "STATISTICS_MINIMUM" of
          Just (String m) -> pure m
          _ -> Left "Failed to decode min"

      mmax <-
        case metadata ^? key "STATISTICS_MAXIMUM" of
          Just (String m) -> pure m
          _ -> Left "Failed to decode max"

      pure (read $ unpack mmin, read $ unpack mmax)

    _ -> do
      mmin <-
        case fst_band ^? key "computedMin" of
          Just (Number x) -> pure x
          _ -> Left "Failed to get metadata and computedMin"

      mmax <-
        case fst_band ^? key "computedMax" of
          Just (Number x) -> pure x
          _ -> Left "Failed to get metadata and computedMax"

      pure (mmin, mmax)

-- | Increasing if spread is negative, decreasing if spread is positive
suhSigmoid :: Double -> Double -> Maybe Double -> String -> String
suhSigmoid midpoint spread (Just d) i =
  "1/((1+((" <> i <> " / (" <> show d <> ")) /" <> show midpoint <> ")^(" <> show spread <> ")))"
suhSigmoid midpoint spread _ i =
  "1/((1+( " <> i <> "/" <> show midpoint <> ")^(" <> show spread <> ")))"

linear :: Double -> Double -> String
linear gradient y_intercept =
  "(" <> show gradient <> ")*A +(" <> show y_intercept <> ")"

linearClamped :: Double -> Double -> Double -> Double -> Double -> Double -> String
linearClamped lv rv gradient y_intercept clamp_left clamp_right =
  show lv
   <> "*(A<"
   <> show clamp_left
   <> ")"
   <> "+("
   <> linear gradient y_intercept
   <> ")*logical_and(A>="
   <> show clamp_left
   <> ", A<="
   <> show clamp_right
   <> ")"
   <> "+ "
   <> show rv
   <> "* (A>"
   <> show clamp_right
   <> ")"

linearClampedInc :: Double -> Double -> Double -> Double -> String
linearClampedInc = linearClamped 0 1

linearClampedDec :: Double -> Double -> Double -> Double -> String
linearClampedDec = linearClamped 1 0

gaussian :: Double -> Double -> Double -> String -> String
gaussian b c div' i =
  e <> "^(- (((" <> i <> "/" <> show div' <> ") - (" <> show b <> ")) ^ 2) / (2 * (" <> show c <> ")^2))"
    where
      e = "2.71828182845904523536"

weightCriteria :: Double -> String -> String -> IO String
weightCriteria w i o = do
  exists <- doesFileExist o
  if w == 1 && exists
    then pure o
    else standardize ("A*" <> show w) i o

-- | Output is the suitability score
multiplyAllCriteria :: [String] -> String -> IO String
multiplyAllCriteria = finalRasterCalculator
