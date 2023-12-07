module Day10 (part1, part2) where
import           Data.List.Split    (chunksOf, splitOn)

import           Data.Sequence      (Seq ((:<|), (:|>)), empty, index,
                                     singleton, (><))

day = 10

executeAll :: Int -> [String] -> Seq Int
executeAll _ [] = empty
executeAll x (i:is) = result >< executeAll reg is
  where
    result = execute x i
    (_ :|> reg) = result

execute :: Int -> String -> Seq Int
execute x "noop" = singleton x
execute x s      = x :<| x + read (words s !! 1) :<| empty

sprite :: Seq Int -> Int -> Char
sprite seq x
  | index seq (x - 1) - 1 <= mod (x - 1) 40 &&
      mod (x - 1) 40 <= index seq (x - 1) + 1 = '#'
  | otherwise = '.'

register :: String -> Seq Int
register input = 1 :<| executeAll 1 (lines input)

part1 :: Bool -> String -> String
part1 _ input = show $ index r 19 * 20 + index r 59 * 60 + index r 99 * 100 + index r 139 * 140 + index r 179 * 180 + index r 219 * 220
      where
        r = register input

part2 :: Bool -> String -> String
part2 _ input = unlines . chunksOf 40 $ [sprite r x | x <- [1..240]]
  where
    r = register input
