module Day11
  ( part1
  , part2
  ) where

import           Data.Char (digitToInt, intToDigit)
import           Data.List as L (filter, groupBy, map, sortBy)
import           Data.Map  as M (Map, adjust, assocs, filter, fromList, keys,
                                 map, null, size, (!))
import           Linear.V2 (V2 (..))

type Pos = V2 Int

type Cavern = Map Pos Int

surroundings = [V2 x y | x <- [(-1) .. 1], y <- [(-1) .. 1]]

buildMap :: String -> Cavern
buildMap string =
  fromList
    [ (V2 x y, digitToInt $ l !! y !! x)
    | x <- [0 .. width - 1]
    , y <- [0 .. height - 1]
    ]
  where
    l = lines string
    width = length . head $ l
    height = length l

energize :: Cavern -> Cavern
energize = M.map (+ 1)

flash :: [Pos] -> Cavern -> Cavern
flash seen cavern
  | all (`elem` seen) newSeen = cavern
  | otherwise = flash newSeen . foldl (flip (adjust (+ 1))) cavern $ toFlash
  where
    flashed = L.filter (`notElem` seen) newSeen
    toFlash = concatMap neighbours flashed
    newSeen = keys . M.filter (> 9) $ cavern

neighbours :: Pos -> [Pos]
neighbours pos = L.map (pos +) surroundings

deplete :: Cavern -> Cavern
deplete =
  M.map
    (\x ->
       if x > 9
         then 0
         else x)

doRound :: (Cavern, Int) -> (Cavern, Int)
doRound (cavern, acc) =
  (deplete flashed, (+ acc) . M.size . M.filter (> 9) $ flashed)
  where
    flashed = flash [] . energize $ cavern

doRounds :: Cavern -> [(Cavern, Int)]
doRounds cavern = iterate doRound (cavern, 0)

render :: Cavern -> String
render =
  unlines .
  L.map (L.map (intToDigit . snd)) . groupBy alignV2 . sortBy compareV2 . assocs
  where
    compareV2 (V2 a b, _) (V2 c d, _)
      | b == d = compare a c
      | otherwise = compare b d
    alignV2 (V2 _ a, _) (V2 _ c, _) = a == c

part1 :: Bool -> String -> String
part1 _ = show . snd . last . take 101 . doRounds . buildMap

part2 :: Bool -> String -> String
part2 _ =
  show .
  length .
  takeWhile (not . M.null . M.filter (/= 0) . fst) . doRounds . buildMap
