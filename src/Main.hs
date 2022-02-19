{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Analysis (
  multiplyAllCriteria,
  rangeStandardize',
  weightCriteria,
 )
import Config.Combined (Config (..), configToCriteria)
import Constraints (multiplyFinalWithConstraint, processConstraints)
import Control.Lens ((&), (.~), (<&>), (?~), (^.))
import Core (Path (path), Raster (Raster), Vector (Vector))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Utils (
  Criterion,
  Input (Path, RequireOutput),
  Require (_r_inputs),
  inputs,
  mkStdName,
  mkWeightedName,
  output,
  prep_f,
  r_output,
  r_prep_f,
  require,
  result,
  std_f,
  weight,
 )

{-# ANN module "HLint: ignore Use camelCase" #-}

main :: IO ()
main = do
  file <- readFile "configs/run.txt"
  mapM_ main' $ words file

main' :: String -> IO ()
main' name = do
  let config_file = "configs" </> (name <> ".json")
  mc <- eitherDecodeFileStrict config_file :: IO (Either String Config)
  case mc of
    Left x -> print x >> error "Decode failed"
    Right c -> do
      let out_dir = "out" </> name
      createDirectoryIfMissing True $ out_dir </> "preprocessed"
      createDirectoryIfMissing True $ out_dir </> "constraints"
      createDirectoryIfMissing True $ out_dir </> "std"
      createDirectoryIfMissing True $ out_dir </> "weighted"

      let (studyArea, cr, cons) = configToCriteria c

      constraints_ <- processConstraints out_dir studyArea cons
      let processed_state = runPreprocessing out_dir studyArea cr
      let standardized_state = runStandardization processed_state
      let weighted_state = runWeights standardized_state
      rasters <- sequence [state ^. result & fromJust | state <- weighted_state]
      final <- multiplyAllCriteria (Raster <$> rasters) $ Raster $ out_dir </> "final.tif"
      final_std <- rangeStandardize' final $ Raster $ out_dir </> "final_std.tif"
      for_ constraints_ $ \c_ ->
        multiplyFinalWithConstraint final_std c_ $ Raster $ out_dir </> "final_clipped.tif"

runPreprocessing :: FilePath -> Vector -> [Criterion] -> [Criterion]
runPreprocessing out_dir border criteria_ = do
  [g border criterion | criterion <- criteria_]
  where
    g :: Vector -> Criterion -> Criterion
    g border_ criterion =
      case criterion ^. require of
        Nothing -> do
          let is = criterion ^. inputs
          -- Force all inputs into strings of filepaths that exist or error
          let msg x =
                "No requirements specified, but criteria with inputs "
                  <> show is
                  <> " depends on the output of requirement"
                  <> show x
          let paths =
                is
                  <&> ( \case
                          Path y -> y
                          RequireOutput x -> error $ msg x
                      )
          criterion & result ?~ h border_ criterion prep_f (const paths) output
        Just req -> do
          -- Set result to make it do req_io first, before doing the actual io
          let req_io = h border_ req r_prep_f _r_inputs r_output
          let is = criterion ^. inputs
          -- Convert requirement output to this outdir (remember that
          -- the outdir depends on the name of the current config file)
          let paths =
                [ case x of
                  Path p -> p
                  RequireOutput o -> out_dir </> "preprocessed" </> o
                | x <- is
                ]
          let this_io = h border_ criterion prep_f (const paths) output
          criterion & result ?~ (req_io >> this_io)

    h border_ obj prep_f_getter in_f_getter out_f_getter =
      prep_f_ (path border_) is out
      where
        prep_f_ = obj ^. prep_f_getter
        is = in_f_getter obj
        out = out_dir </> "preprocessed" </> obj ^. out_f_getter

abstract :: (Criterion -> String -> IO String) -> [Criterion] -> [Criterion]
abstract func criteria_ =
  [ criterion & result .~ f criterion
  | criterion <- criteria_
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
  abstract (\criterion r -> weightCriteria' (criterion ^. weight) r $ mkWeightedName r)

weightCriteria' :: Double -> String -> String -> IO String
weightCriteria' w i o = path <$> weightCriteria w (Raster i) (Raster o)
