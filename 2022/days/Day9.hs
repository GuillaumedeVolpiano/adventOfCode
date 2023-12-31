module Day9 (part1, part2) where
import           Data.List.Split    (splitOn)

import           Data.Char          (digitToInt)
import           Data.List          (nub)
import           Data.Map           as M (Map, fromList, (!))
import           Data.Set           as S (fromList, size)
import           Linear             (V2 (..))

day = 9

moves =
  M.fromList [('R', V2 1 0), ('L', V2 (-1) 0), ('U', V2 0 (-1)), ('D', V2 0 1)]

shortRope = replicate 2 (V2 0 0)

longRope = replicate 10 (V2 0 0)

move :: V2 Int -> Char -> V2 Int
move pos c = pos + moves ! c

follow :: V2 Int -> V2 Int -> V2 Int
follow h t
  | abs x < 2 && abs y < 2 = t
  | otherwise = t + V2 (sig x) (sig y)
  where
    (V2 x y) = h - t
    sig x
      | x < 0 = -1
      | x > 0 = 1
      | x == 0 = 0
    sig y
      | y < 0 = -1
      | y > 0 = 1
      | y == 0 = 0

moveRope :: V2 Int -> [V2 Int] -> [V2 Int]
moveRope h [] = []
moveRope h (n:ns) = nn : moveRope nn ns
  where
    nn = follow h n

fullMove :: [V2 Int] -> Char -> [V2 Int]
fullMove (h:t) c = nh : moveRope nh t
  where
    nh = move h c

movements = concatMap (\(x:_:y) -> replicate (read y) x) . lines

part1 :: Bool -> String -> String
part1 _ = show . size . S.fromList . map last . scanl fullMove shortRope . movements

part2 :: Bool -> String -> String
part2 _ = show . size . S.fromList . map last . scanl fullMove longRope . movements
