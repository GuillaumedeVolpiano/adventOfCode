module Day12
  ( part1
  , part2
  ) where

import           Control.Lens.Getter ((^.))
import           Data.Sequence       (fromList)
import           Helpers.Parsers     (integers)
import           Helpers.Search      (findPattern)
import           Linear.V2           (V2 (..))
import           Linear.V3           (V3 (..), _x, _y, _z)
import           Linear.Vector       (Additive)

data Moon =
  Moon Pos Vel

type Pos = V3 Int

type Vel = V3 Int

checkPatterns :: [[Moon]] -> Int
checkPatterns moonRounds = foldr1 lcm patterns
  where
    patterns =
      concatMap
        (\x ->
           map
             (\v ->
                findPattern 0 1000 (==) . fromList . map (x . (!! v)) $
                moonRounds)
             [0 .. 3])
        [projectX, projectY, projectZ]

projectX :: Moon -> V2 Int
projectX (Moon p v) = V2 (p ^. _x) (v ^. _x)

projectY :: Moon -> V2 Int
projectY (Moon p v) = V2 (p ^. _y) (v ^. _y)

projectZ :: Moon -> V2 Int
projectZ (Moon p v) = V2 (p ^. _z) (v ^. _z)

doRound :: [Moon] -> [Moon]
doRound moons = map (\x -> velocity . foldr gravity x $ moons) moons

velocity :: Moon -> Moon
velocity (Moon p v) = Moon (p + v) v

gravity :: Moon -> Moon -> Moon
gravity (Moon (V3 a b c) _) (Moon (V3 x y z) vel) =
  Moon (V3 x y z) (vel + V3 (signum (a - x)) (signum (b - y)) (signum (c - z)))

totalEnergy :: Moon -> Int
totalEnergy (Moon (V3 x y z) (V3 dx dy dz)) =
  (abs x + abs y + abs z) * (abs dx + abs dy + abs dz)

makeMoons :: String -> [Moon]
makeMoons = map (\[x, y, z] -> Moon (V3 x y z) (V3 0 0 0)) . integers

part1 :: Bool -> String -> String
part1 test =
  show .
  sum . map totalEnergy . last . take (n + 1) . iterate doRound . makeMoons
  where
    n
      | test = 10
      | otherwise = 1000

part2 :: Bool -> String -> String
part2 _ = show . checkPatterns . take 500001 . iterate doRound . makeMoons
