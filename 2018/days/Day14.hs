module Day14
  ( part1
  , part2
  ) where

import           Data.Char     (digitToInt, intToDigit)
import           Data.Sequence as S (Seq ((:|>)), drop, fromList, index, length,
                                     take)
import           Debug.Trace

initialState = fromList [3, 7]

initialPos1 = 0

initialPos2 = 1

cookTo :: Seq Int -> Int -> Int -> Int -> String
cookTo recipes fe se num
  | S.length newRecipes >= num + 10 =
    foldr (\a b -> intToDigit a : b) "" . S.take 10 . S.drop num $ newRecipes
  | otherwise = cookTo newRecipes nfe nse num
  where
    (newRecipes, nfe, nse) = cook recipes fe se

cook :: Seq Int -> Int -> Int -> (Seq Int, Int, Int)
cook recipes fe se = (newRecipes, nfe, nse)
  where
    fr = index recipes fe --The first elf's recipe
    sr = index recipes se --The second elf's recipe
    intRecipes
      | fr + sr >= 10 = recipes :|> 1
      | otherwise = recipes
    newRecipes = intRecipes :|> mod (fr + sr) 10
    nfe = mod (1 + fe + fr) . S.length $ newRecipes
    nse = mod (1 + se + sr) . S.length $ newRecipes

cookLeft :: Seq Int -> Int -> Int -> Seq Int -> Int
cookLeft recipes fe se num
  | lr >= ln && (S.take ln . S.drop (lr - ln) $ recipes) == num = lr - ln
  | lr >= ln + 1 && (S.take ln . S.drop (lr - ln - 1) $ recipes) == num =
    lr - ln - 1
  | otherwise = cookLeft newRecipes nfe nse num
  where
    (newRecipes, nfe, nse) = cook recipes fe se
    lr = S.length recipes
    ln = S.length num

part1 :: Bool -> String -> String
part1 _ = cookTo initialState initialPos1 initialPos2 . read . init

part2 :: Bool -> String -> String
part2 _ =
  show .
  cookLeft initialState initialPos1 initialPos2 .
  fromList . map digitToInt . init
