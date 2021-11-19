{-# LANGUAGE RankNTypes #-}

module Main where

import Analysis (
  weightCriteria, multiplyAllCriteria, rangeStandardize
 )
import Config (Config (..), configToCriteria, CriterionConfig (CriterionConfig))
import qualified Config as C
import Control.Lens ((&), (.~), (?~), (^.))
import Preprocessing.Core (filterVectorByField, cropRasterWithBorder, bufferVector)
import Utils
import Data.Aeson (decodeFileStrict', encodeFile)
import System.Directory (createDirectoryIfMissing)
import Data.Maybe (fromJust)
import System.FilePath ((</>))
import Constraints (processConstraints, multiplyFinalWithConstraint)

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = do
  file <- readFile "configs/run.txt"
  mapM_ main' $ words file

main' :: String -> IO ()
main' name = do
  mc <- (decodeFileStrict' $ "configs/" <> name <> ".json" :: IO (Maybe Config))
  case mc of
    Nothing -> error "Decode failed"
    Just c -> do
      let out_dir = "out" </> name
      createDirectoryIfMissing True $ out_dir </> "preprocessed"
      createDirectoryIfMissing True $ out_dir </> "constraints"
      createDirectoryIfMissing True $ out_dir </> "std"
      createDirectoryIfMissing True $ out_dir </> "weighted"

      let (cr, cons) = configToCriteria c

      border <- cropBorder out_dir
      constraints <- processConstraints out_dir border cons
      let processed_state = runPreprocessing out_dir border cr
      let standardized_state = runStandardization processed_state
      let weighted_state = runWeights standardized_state
      rasters <- sequence [ state^.result & fromJust | state <- weighted_state]
      -- TODO: range standardize
      final <- multiplyAllCriteria rasters $ out_dir </> "final.tif"
      final_std <- rangeStandardize final $ out_dir </> "final_std.tif"
      case constraints of
        Nothing -> pure ()
        Just c -> do
          _ <- multiplyFinalWithConstraint
            final_std c $ out_dir </> "final_clipped.tif"
          pure ()
      pure ()

cropBorder :: FilePath -> IO String
cropBorder out_dir =
  filterVectorByField
    "NAME_1"
    "Arizona"
    "../data/borders/gadm36_USA_1.shp" $
    out_dir </> "az border.shp"

runPreprocessing :: FilePath -> String -> [Criterion] -> [Criterion]
runPreprocessing out_dir border criteria = do
  [ g border criterion | criterion <- criteria ]
  where
    g border_ criterion =
      case criterion ^. require of
        Nothing -> criterion & result ?~ h border_ criterion prep_f inputs output
        Just req -> do
          -- Set result to make it do req_io first, before doing the actual io
          -- Ignores the result of req_io as it is not a criteria but merely a step
          -- TODO let criteria take inputs from requirement output
          let req_io = h border_ req r_prep_f r_inputs r_output
          let this_io = h border_ criterion prep_f inputs output
          criterion & result ?~ (req_io >> this_io)

    h border_ obj prep_f_getter in_f_getter out_f_getter =
      prep_f_ border_ is out
        where
          prep_f_ = obj ^. prep_f_getter
          is = obj ^. in_f_getter
          out = out_dir </> "preprocessed" </> obj ^. out_f_getter

abstract :: (Criterion -> String -> IO String) -> [Criterion] -> [Criterion]
abstract func criteria =
  [ criterion & result .~ f criterion
  | criterion <- criteria
  ]
  where
    f criterion =
      case criterion ^. result of
        Nothing -> Nothing
        Just io_raster ->
          Just $ do
            r <- io_raster
            func criterion r

runStandardization :: [Criterion] -> [Criterion]
runStandardization =
  abstract (\criterion r -> (criterion ^. std_f) r $ mkStdName r)

runWeights :: [Criterion] -> [Criterion]
runWeights =
  abstract (\criterion r -> weightCriteria (criterion ^. weight) r $ mkWeightedName r)
