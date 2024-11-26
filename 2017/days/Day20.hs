module Day20
  ( part1
  , part2
  ) where

import           Data.Bifunctor  (second)
import           Data.List       (groupBy, minimumBy, sortBy)
import           Data.Ord        (comparing)
import           Helpers.Parsers (numbers)
import           Linear.V3       (V3 (..))

import           Debug.Trace

data Particle =
  Particle Pos Velocity Acceleration
  deriving (Show)

type Pos = V3 Int

type Velocity = V3 Int

type Acceleration = V3 Int

type Space = [(Int, Particle)]

type Particles = [Particle]

expand :: Space -> Int
expand space
  | all (isMovingAway . snd) space =
    fst . minimumBy (comparing snd) . map (second expansionSpeed) $ space
  | otherwise = expand . fmap (second move) $ space

move :: Particle -> Particle
move (Particle pos vel acc) = Particle pos' vel' acc
  where
    vel' = vel + acc
    pos' = pos + vel'

collide :: Particles -> Int
collide particles
  | all isMovingAway particles = length particles
  | otherwise =
    collide
      . concat
      . filter ((== 1) . length)
      . groupBy (\a b -> position a == position b)
      . sortBy (comparing position)
      . map move
      $ particles

position :: Particle -> Pos
position (Particle p _ _) = p

isMovingAway :: Particle -> Bool
isMovingAway (Particle (V3 x y z) (V3 dx dy dz) (V3 ddx ddy ddz)) =
  (signum x == signum dx || (signum dx == 0 && signum ddx == 0))
    && (signum dx == signum ddx || signum ddx == 0)
    && (signum y == signum dy || (signum dy == 0 && signum ddy == 0))
    && (signum dy == signum ddy || signum ddy == 0)
    && (signum z == signum dz || (signum dz == 0 && signum ddz == 0))
    && (signum dz == signum ddz || signum ddz == 0)

expansionSpeed :: Particle -> Int
expansionSpeed (Particle _ (V3 dx dy dz) _) = abs dx + abs dy + abs dz

makeParticle :: [Int] -> Particle
makeParticle [x, y, z, dx, dy, dz, ddx, ddy, ddz] =
  Particle (V3 x y z) (V3 dx dy dz) (V3 ddx ddy ddz)

part1 :: Bool -> String -> String
part1 _ = show . expand . zip [0 ..] . map makeParticle . numbers

part2 :: Bool -> String -> String
part2 _ = show . collide . map makeParticle . numbers
