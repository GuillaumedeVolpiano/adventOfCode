module Day2
  ( part1
  , part2
  ) where

import           Data.Sequence  as Sq (Seq ((:<|), (:|>)), empty, fromList,
                                       null, (><))
import           Data.Sequences (IsSequence, tailEx)
import           Data.Zip       as Z (Zip, zipWith)

isSafe :: Foldable t => t Int -> Bool
isSafe diffs =
  (all (> 0) diffs && not (any (> 3) diffs))
    || (all (< 0) diffs && not (any (< (-3)) diffs))

dampenedSafe :: Seq Int -> Bool
dampenedSafe (l :<| evels) = dampen empty l evels

dampen :: Seq Int -> Int -> Seq Int -> Bool
dampen xs x ys
  | Sq.null xs = checkSafe ys || dampen (x :<| empty) y ys'
  | Sq.null ys = checkSafe xs
  | otherwise = checkSafe (xs >< ys) || dampen (xs :|> x) y ys'
  where
    (y :<| ys') = ys

checkSafe :: (Foldable t, Zip t, IsSequence (t Int)) => t Int -> Bool
checkSafe levels = isSafe . Z.zipWith (-) levels $ tailEx levels

part1 :: Bool -> String -> String
part1 _ = show . length . filter checkSafe . map (map read . words) . lines

part2 :: Bool -> String -> String
part2 _ =
  show
    . length
    . filter dampenedSafe
    . map (fromList . map read . words)
    . lines
