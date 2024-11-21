module Day14
  ( part1
  , part2
  ) where

import           Data.Char     (digitToInt, intToDigit)
import           Data.List     as L (isPrefixOf, length, tails)
import           Data.Sequence as S (fromList, index, length, (><))

initialList = [3, 7]

initialState = fromList initialList

initialPos1 = 0

initialPos2 = 1

recipes :: [Int]
recipes = 3 : 7 : cookLazy initialPos1 initialPos2 initialState
  where
    cookLazy fe se re = newRecipes ++ cookLazy nfe nse nre
      where
        fr = index re fe
        sr = index re se
        newRecipes = splitTens (fr + sr)
        nre = re >< fromList newRecipes
        nfe = mod (1 + fe + fr) (S.length nre)
        nse = mod (1 + se + sr) (S.length nre)

splitTens :: Int -> [Int]
splitTens rec
  | rec < 10 = [rec]
  | otherwise = [div rec 10, mod rec 10]

part1 :: Bool -> String -> String
part1 _ input = map intToDigit . take 10 . drop (read . init $ input) $ recipes

part2 :: Bool -> String -> String
part2 _ input =
  show . L.length . takeWhile (not . (number `isPrefixOf`)) . tails $ recipes
  where
    number = map digitToInt . init $ input
