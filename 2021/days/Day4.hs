module Day4
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, array, bounds, indices, (!))
import           Data.List          (inits)
import           Data.List.Split    (splitWhen)
import           Data.Maybe         (Maybe (Just, Nothing), mapMaybe)
import           Helpers.Parsers    (integers, make2DArray)
import           Linear.V2          (V2 (..))

type Board = UArray Pos Int

type Pos = V2 Int

checkWins :: [Board] -> [Int] -> Maybe ([Int], Board)
checkWins [] _ = Nothing
checkWins (b:oard) draw
  | wins draw b = Just (draw, b)
  | otherwise = checkWins oard draw

checkLose :: [Board] -> [Int] -> [Board]
checkLose boards draw = filter (not . wins draw) boards

wins :: [Int] -> Board -> Bool
wins draw b = winX || winY
  where
    (_, V2 mx my) = bounds b
    winX =
      any (all (\p -> (b ! p) `elem` draw)) $
      [[V2 x y | y <- [0 .. my]] | x <- [0 .. mx]]
    winY =
      any (all (\p -> (b ! p) `elem` draw)) $
      [[V2 x y | x <- [0 .. mx]] | y <- [0 .. my]]

score :: ([Int], Board) -> Int
score (draw, board) =
  last draw *
  (sum . map (board !) . filter (\p -> (board ! p) `notElem` draw) . indices $
   board)

findDraw :: [Int] -> Board -> ([Int], Board)
findDraw draws board =
  (head . dropWhile (\x -> not . wins x $ board) . inits $ draws, board)

part1 :: Bool -> String -> String
part1 _ input =
  show . score . head . mapMaybe (checkWins boards) . inits $ draws
  where
    rawBingo = integers input
    draws = head rawBingo
    boards = map make2DArray . splitWhen null . drop 2 $ rawBingo

part2 :: Bool -> String -> String
part2 _ input =
  show .
  score .
  findDraw draws .
  head . head . filter (\x -> length x == 1) . map (checkLose boards) . inits $
  draws
  where
    rawBingo = integers input
    draws = head rawBingo
    boards = map make2DArray . splitWhen null . drop 2 $ rawBingo
    losingBoard =
      head . filter (\x -> length x == 1) . map (checkLose boards) . inits $
      draws
