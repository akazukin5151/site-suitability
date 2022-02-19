{-# LANGUAGE LambdaCase #-}

module Constraints where

import Control.Lens ((<&>), (^.))
import Core (Path (path), Raster (Raster), Vector, finalRasterCalculator)
import System.FilePath ((</>))
import Utils (Constraint, Input (Path, RequireOutput), c_func, c_inputs, c_output, c_require, guardFileF, quoteDouble, quoteSingle, r_inputs, r_output, r_prep_f, runCmd)

processConstraints :: FilePath -> Vector -> [Constraint] -> IO (Maybe Raster)
processConstraints _ _ []              = pure Nothing
processConstraints out_dir border cons = do
  rasts <- sequence [g border constraint | constraint <- cons]
  r <- finalRasterCalculator rasts out
  pure $ Just r
  where
    out = Raster $ out_dir </> "constraints.tif"

    g :: Vector -> Constraint -> IO Raster
    g b constraint =
      case constraint ^. c_require of
        Nothing -> do
          let is = constraint ^. c_inputs
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
          h b constraint c_func (const paths) c_output
        Just req -> do
          _ <- h b req r_prep_f (^. r_inputs) r_output
          let is = constraint ^. c_inputs
          let paths =
                [ case x of
                  Path p -> p
                  RequireOutput o -> out_dir </> "constraints" </> o
                | x <- is
                ]
          h b constraint c_func (const paths) c_output

    h b a pf inf outf = Raster <$> f (path b) is out_
      where
        f = a ^. pf
        is = inf a
        out_ = out_dir </> "constraints" </> a ^. outf

multiplyFinalWithConstraint :: Raster -> Raster -> Raster -> IO Raster
multiplyFinalWithConstraint (Raster f) (Raster c) (Raster out) = do
  guardFileF Raster out $
    runCmd
      "qgis_process"
      [ "run"
      , "qgis:rastercalculator"
      , "--"
      , "CELLSIZE=0"
      , "CRS='EPSG:4326'"
      , "EXPRESSION='\"" <> f <> "@1\" * \"" <> c <> "@1\"'"
      , -- extents seem to be optional
        "OUTPUT=" <> quoteSingle out
      , "LAYERS=" <> quoteDouble f
      , "LAYERS=" <> quoteDouble c
      ]
