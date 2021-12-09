{-# LANGUAGE LambdaCase #-}
module Constraints where

import Utils (Constraint, c_require, r_prep_f, r_inputs, r_output, c_func, c_inputs, c_output, guardFile, runCmd, quoteSingle, quoteDouble, Input (Path, RequireOutput))
import Control.Lens ((^.), (<&>))
import System.FilePath ((</>))
import Core (finalRasterCalculator)


processConstraints :: FilePath -> String -> [Constraint] -> IO (Maybe FilePath)
processConstraints out_dir border cons =
  case cons of
    [] -> pure Nothing
    _ -> do
          rasts <- sequence [ g border constraint | constraint <- cons ]
          r <- finalRasterCalculator rasts out
          pure $ Just r
  where
    out = out_dir </> "constraints.tif"
    g b constraint =
      case constraint ^. c_require of
        Nothing -> do
          let is = constraint ^. c_inputs
          -- Force all inputs into strings of filepaths that exist or error
          let msg x =
                "No requirements specified, but criteria with inputs "
                <> show is <> " depends on the output of requirement" <> show x
          let paths = is <&> (\case
                Path y -> y
                RequireOutput x -> error $ msg x)
          h b constraint c_func (const paths) c_output

        Just req -> do
          _ <- h b req r_prep_f (^. r_inputs) r_output
          let is = constraint ^. c_inputs
          let paths = [ case x of
                          Path p -> p
                          RequireOutput o -> out_dir </> "constraints" </> o
                      | x <- is
                      ]
          h b constraint c_func (const paths) c_output

    h b a pf inf outf = f b is out
      where
        f = a ^. pf
        is = inf a
        out = out_dir </> "constraints" </> a ^. outf

multiplyFinalWithConstraint :: String -> String -> FilePath -> IO FilePath
multiplyFinalWithConstraint f c out = do
  guardFile out $
    runCmd "qgis_process"
      [ "run"
      , "qgis:rastercalculator"
      , "--"
      , "CELLSIZE=0"
      , "CRS='EPSG:4326'"
      , "EXPRESSION='\"" <> f <> "@1\" * \"" <> c <> "@1\"'"
      -- extents seem to be optional
      , "OUTPUT=" <> quoteSingle out
      , "LAYERS=" <> quoteDouble f
      , "LAYERS=" <> quoteDouble c
      ]
  pure out
