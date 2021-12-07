module Constraints where

import Utils (Constraint, c_require, r_prep_f, r_inputs, r_output, prep_f, c_func, c_inputs, c_output, appendFilename, guardFile, runCmd, quoteSingle, quoteDouble)
import Control.Lens ((^.), (?~), (&))
import System.FilePath ((</>))
import Preprocessing.Combined
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
        Nothing -> def
        Just req -> do
          _ <- h b req r_prep_f r_inputs r_output
          def
      where
        def = h b constraint c_func c_inputs c_output

    h b a pf inf outf = f b is out
      where
        f = a ^. pf
        is = a ^. inf
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
      -- TODO get extents; got it from insolation, which should be same as border
      --, "EXTENT='-114.808333333,-109.050000000,31.333333333,37.000000000'"
      , "OUTPUT=" <> quoteSingle out
      , "LAYERS=" <> quoteDouble f
      , "LAYERS=" <> quoteDouble c
      ]
  pure out
