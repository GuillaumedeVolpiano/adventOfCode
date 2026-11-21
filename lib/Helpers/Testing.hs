-- |
-- Module      : Helpers.Testing
-- Description : The example test suite shared by every year.
--
-- Each part whose example answer is known is run on test/<year>/dayN.txt
-- and checked against it. Part 2 reads test/<year>/dayNb.txt instead when
-- that file exists, for puzzles that give a second example. Parts whose
-- answer is still Nothing are skipped.
module Helpers.Testing
  ( testYear
  ) where

import           Control.Monad              (when)
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IM (elems, toList)
import           Data.Maybe                 (isNothing)
import qualified Data.ByteString            as BS (readFile)
import           System.Directory           (doesFileExist)
import qualified System.Exit                as Exit
import           System.FilePath            (takeDirectory, (</>))
import           Test.HUnit

import           Helpers.Day                (Day (Day), Solver)
import           Helpers.Input              (fromByteString)
import           Helpers.Repository         (testFile)

testYear :: Int -> IntMap Day -> IO ()
testYear year days = do
  result <- runTestTT . TestList . map (uncurry (dayTests year)) $ IM.toList days
  let unknown = sum . map countUnknown $ IM.elems days
  when (unknown > 0) . putStrLn $
    show unknown ++ " part(s) skipped: example answer not filled in yet."
  if errors result + failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess

countUnknown :: Day -> Int
countUnknown (Day _ e1 _ e2) = length . filter id $ [isNothing e1, isNothing e2]

dayTests :: Int -> Int -> Day -> Test
dayTests year n (Day solve1 expected1 solve2 expected2) =
  TestList [part year n 1 solve1 expected1, part year n 2 solve2 expected2]

part :: (Eq a, Show a) => Int -> Int -> Int -> Solver a -> Maybe a -> Test
part _ _ _ _ Nothing = TestList []
part year n p solve (Just expected) =
  TestLabel ("day " ++ show n ++ ", part " ++ show p) . TestCase $ do
    file <- exampleFile year n p
    exists <- doesFileExist file
    if exists
      then do
        result <- BS.readFile file >>= solve True . fromByteString
        assertEqual "" expected result
      else assertFailure $ "missing example file " ++ file

exampleFile :: Int -> Int -> Int -> IO FilePath
exampleFile year n p = do
  first <- testFile year n
  let second = takeDirectory first </> ("day" ++ show n ++ "b.txt")
  hasSecond <- doesFileExist second
  pure $
    if p == 2 && hasSecond
      then second
      else first
