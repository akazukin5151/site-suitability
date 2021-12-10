{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils where

import Control.Monad (foldM, unless)
import Lens.Micro.TH (makeLenses)
import System.Directory (doesFileExist)
import System.Exit (ExitCode)
import System.FilePath (replaceBaseName, takeBaseName, takeDirectory, (</>), takeExtension, (<.>))
import System.Process (callCommand, readProcessWithExitCode)

data Input = Path String
           | RequireOutput String
           deriving Show

data Criterion =
  Criterion { _name :: String
            , _inputs :: [Input]
            , _output :: String
            , _prep_f :: String -> [String] -> String -> IO String
            -- ^         Border -> [input] -> output -> IO preprocessed
            , _std_f  :: String -> String -> IO String
            , _weight :: Double
            , _result :: Maybe (IO String)
            -- ^ TODO: This should really be in another type like ProcessedCriterion
            -- and no real reason to keep it inside the IO monad...
            , _require :: Maybe Require
            -- ^ Other criteria that must be pre-processed before processing this
            -- criteria, but they themselves should not count towards the final score
            }

data Require =
  Require { _r_name   :: String
          , _r_inputs :: [String]
          , _r_output :: String
          , _r_prep_f :: String -> [String] -> String -> IO String
          }

makeLenses ''Require
makeLenses ''Criterion

data Constraint =
  Constraint { _c_name :: String
             , _c_inputs :: [Input]
             , _c_output :: String
             , _c_func :: String -> [String] -> FilePath -> IO FilePath
             -- ^ Should always return a raster
             , _c_require :: Maybe Require
             }

makeLenses ''Constraint

data ShouldRemoveStepDir = RemoveStepDir | DontRemoveStepDir
  deriving (Eq)

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x : xs) = foldM f x xs
foldM1 _ _ = error "Empty list in foldM1"

quoteSingle :: String -> String
quoteSingle s = "'" <> s <> "'"

quoteDouble :: String -> String
quoteDouble s = "\"" <> s <> "\""

runCmd :: String -> [String] -> IO ()
runCmd process args = do
  print $ unwords $ [process] <> args
  callCommand $ unwords $ [process] <> args

readCmd :: FilePath -> [String] -> IO (ExitCode, String, String)
readCmd process args = do
  print $ unwords $ [process] <> args
  readProcessWithExitCode process args ""

-- mkStdName "out/y/preprocessed/x.shp" == "out/y/std/x_std.shp"
mkStdName :: FilePath -> FilePath
mkStdName x =
  new_dir </> new_name
    where
      new_name = takeBaseName x <> "_std" <.> takeExtension x  -- "x_std.shp"
      dir = takeDirectory x                                    -- "out/x/preprocessed"
      new_dir = takeDirectory dir </> "std"                    -- "out/x/std"

-- mkWeightedName "out/y/std/x.shp" == "out/y/weighted/x_weighted.shp"
mkWeightedName :: FilePath -> FilePath
mkWeightedName x =
  new_dir </> new_name
    where
      new_name =
        takeBaseName x <> "_weighted" <.> takeExtension x  -- "x_weighted.shp"
      dir = takeDirectory x                                -- "out/x/std"
      new_dir = takeDirectory dir </> "weighted"           -- "out/x/weighted"

appendFilename :: String -> FilePath -> FilePath
appendFilename name_ innerFp = do
  let base = takeBaseName innerFp
  let new = base <> name_
  replaceBaseName innerFp new

sequentialFilenames :: Foldable t => String -> t a -> String -> [String]
sequentialFilenames step_dir is ext =
  [ step_dir <> "/step_" <> show idx <> ext
  | idx <- [0 .. length is - 1]
  ]

guardFile :: FilePath -> IO () -> IO ()
guardFile o x = do
  exists <- doesFileExist o
  -- unless x = when $ not x
  unless exists x

guardFile' :: FilePath -> IO () -> IO FilePath
guardFile' o x = do
  guardFile o x
  pure o
