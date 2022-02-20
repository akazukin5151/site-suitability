module Preprocessing.Core where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import Utils (
  ShouldRemoveStepDir (RemoveStepDir),
 )
import System.FilePath ((</>))

stepWrapper :: ShouldRemoveStepDir -> String -> String -> (String -> IO a) -> IO a
stepWrapper should_remove config_name f_name f = do
  let step_dir = "out" </> config_name </> "internal_steps_" <> f_name
  createDirectoryIfMissing True step_dir
  res <- f step_dir
  when (should_remove == RemoveStepDir) $
    removePathForcibly step_dir
  pure res
