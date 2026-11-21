-- |
-- Module      : Helpers.Repository
-- Description : Where the repository lives, and how it introduces itself.
--
-- Inputs, examples, cookies and proxy settings are all found relative to
-- the root of the repository, so that it works wherever it is cloned.
module Helpers.Repository
  ( aocRoot
  , rootFile
  , inputFile
  , testFile
  , userAgent
  ) where

import           System.Directory   (doesFileExist, getCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath    (takeDirectory, (</>))

-- | The root of the repository: AOC_ROOT when it is set, otherwise the
-- first directory holding adventOfCode.cabal, from the current directory
-- upwards.
aocRoot :: IO FilePath
aocRoot = lookupEnv "AOC_ROOT" >>= maybe (getCurrentDirectory >>= climb) pure
  where
    climb dir = do
      found <- doesFileExist (dir </> "adventOfCode.cabal")
      let parent = takeDirectory dir
      if found
        then pure dir
        else if parent == dir
               then fail
                      "adventOfCode.cabal not found above the current directory; set AOC_ROOT"
               else climb parent

-- | A file given relative to the root of the repository.
rootFile :: FilePath -> IO FilePath
rootFile name = (</> name) <$> aocRoot

-- | input/<year>/day<day>.txt
inputFile :: Int -> Int -> IO FilePath
inputFile year day = rootFile $ "input" </> show year </> dayFile day

-- | test/<year>/day<day>.txt
testFile :: Int -> Int -> IO FilePath
testFile year day = rootFile $ "test" </> show year </> dayFile day

dayFile :: Int -> FilePath
dayFile day = "day" ++ show day ++ ".txt"

-- | Advent of Code asks automated tools to identify themselves.
userAgent :: String
userAgent = "github.com/GuillaumedeVolpiano/adventOfCode"
