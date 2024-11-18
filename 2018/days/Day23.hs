module Day23
  ( part1
  , part2
  ) where

import           Data.Hashable        (Hashable, hashWithSalt)
import           Data.HashPSQ         as H (HashPSQ, insert, minView, null,
                                            singleton)
import           Data.List            (maximumBy)
import           Data.Ord             (comparing)
import           Helpers.Parsers      (Parser, nums, parseByLine)
import           Linear.V3            (V3 (..))
import           Text.Megaparsec.Char (char, eol, string)

data Nanobot = Nanobot
  { pos   :: Pos
  , range :: Range
  } deriving (Show, Ord, Eq)

type Pos = V3 Int

type Range = Int

-- With ((V3 ux uy uz), (V3 lx ly lz)), we must have ux < lx, uy < ly, uz < lz.
-- This is not strictly enforced.
data Cube = Cube
  { ul :: UpperLeft
  , lr :: LowerRight
  } deriving (Show, Ord, Eq)

type UpperLeft = Pos

type LowerRight = Pos

data Priority = P
  { nbots :: Int
  , cdist :: Int
  } deriving (Show, Eq)

instance Ord Priority where
  compare p1 p2 =
    compare (nbots p2) (nbots p1) `mappend` compare (cdist p1) (cdist p2)

instance Hashable Cube where
  hashWithSalt salt (Cube uc lc) = hashWithSalt salt uc * hashWithSalt salt lc

class Ranged a where
  inRange :: a -> Nanobot -> Bool

-- first bot is the reference bot, second bot is the one being tested
instance Ranged Nanobot where
  inRange ref test = distance (pos ref) (pos test) <= range ref

instance Ranged Cube where
  inRange (Cube (V3 ux uy uz) (V3 lx ly lz)) (Nanobot (V3 bx by bz) r) =
    x + y + z <= r
    where
      x
        | bx < ux = ux - bx
        | bx > lx = bx - lx
        | otherwise = 0
      y
        | by < uy = uy - by
        | by > ly = by - ly
        | otherwise = 0
      z
        | bz < uz = uz - bz
        | bz > lz = bz - lz
        | otherwise = 0

manMagnitude :: Pos -> Int
manMagnitude (V3 x y z) = abs x + abs y + abs z

distance :: Pos -> Pos -> Int
distance p = manMagnitude . (-) p

cubeDist :: Cube -> Int
cubeDist (Cube a b) = min (manMagnitude a) (manMagnitude b)

parseBot :: Parser Nanobot
parseBot = do
  string "pos=<"
  Just x <- nums
  char ','
  Just y <- nums
  char ','
  Just z <- nums
  string ">, r="
  Just r <- nums
  eol
  return . Nanobot (V3 x y z) $ r

checkBotRange :: [Nanobot] -> Int
checkBotRange bots = flip rangeSize bots . maximumBy (comparing range) $ bots

rangeSize :: Ranged a => a -> [Nanobot] -> Int
rangeSize a = length . filter (inRange a)

largeCube :: [Nanobot] -> Cube
largeCube bots = Cube (V3 (-m) (-m) (-m)) (V3 m m m)
  where
    m =
      (2 ^)
        . ceiling
        . logBase 2
        . fromIntegral
        . maximum
        . map ((\(V3 x y z) -> max (max x y) z) . pos)
        $ bots

findBestPoint :: [Nanobot] -> Int
findBestPoint bots =
  manMagnitude . splitCubes bots . singleton cube (P (length bots) 0) $ cube
  where
    cube = largeCube bots

splitCubes :: [Nanobot] -> HashPSQ Cube Priority Cube -> Pos
splitCubes bots queue
  | H.null queue = error "the queue is somehow empty"
  | ul curCube == lr curCube = ul curCube
  | otherwise = splitCubes bots newQueue
  where
    Just (curCube, _, _, rest) = minView queue
    newCubes = splitCube curCube
    newQueue = foldr (\c q -> insert c (priority c) c q) rest newCubes
    priority c = P (rangeSize c bots) (cubeDist c)

splitCube :: Cube -> [Cube]
splitCube (Cube (V3 ux uy uz) (V3 lx ly lz))
  | lx - ux == 1 =
    [Cube (V3 x y z) (V3 x y z) | x <- [ux, lx], y <- [uy, ly], z <- [uz, lz]]
  | otherwise =
    [ Cube
      (V3 (ux + dux) (uy + duy) (uz + duz))
      (V3 (lx - dlx) (ly - dly) (lz - dlz))
    | dux <- [0, mx]
    , duy <- [0, my]
    , duz <- [0, mz]
    , dlx <- [0, mx]
    , dlx + dux == mx
    , dly <- [0, my]
    , dly + duy == my
    , dlz <- [0, mz]
    , dlz + duz == mz
    ]
  where
    mx = (lx - ux) `div` 2
    my = (ly - uy) `div` 2
    mz = (lz - uz) `div` 2

part1 :: Bool -> String -> String
part1 _ = show . checkBotRange . parseByLine parseBot

part2 :: Bool -> String -> String
part2 _ = show . findBestPoint . parseByLine parseBot
