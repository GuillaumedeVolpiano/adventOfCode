module Day11
  ( part1
  , part2
  ) where

import           Data.Char (digitToInt)
import           Data.List as L (map)
import           Data.Map  as M (Map, adjust, filter, fromList, keys, map, null,
                                 size, (!))
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

flash :: Cavern -> Cavern
flash cavern
  | M.null . M.filter (== 10) $ cavern = cavern
  | otherwise = flash . foldl (flip (adjust (+ 1))) cavern $ toFlash
  where
    flashed = keys . M.filter (== 10) $ cavern
    toFlash = concatMap neighbours flashed

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
    flashed = flash . energize $ cavern

doRounds :: Cavern -> [(Cavern, Int)]
doRounds cavern = iterate doRound (cavern, 0)

part1 :: Bool -> String -> String
part1 _ = show . snd . last . take 11 . doRounds . buildMap

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
