module Day22
  ( part1
  , part2
  ) where

import           Data.List       (sortBy)
import           Helpers.Parsers (complexParser)
import           Linear.V3       (V3 (..))

type Pos = V3 Int

type Brick = (Pos, Pos)

toPos :: [String] -> Pos
toPos [a, b, c] = V3 (read a) (read b) (read c)

toBricks :: [Pos] -> Brick
toBricks [x, y] = (x, y)

lowPointSort :: Brick -> Brick -> Ordering
lowPointSort (V3 _ _ x, _) (V3 _ _ y, _) = compare x y

highPoint :: Brick -> Int
highPoint (_, V3 _ _ z) = z

lowPoint :: Brick -> Int
lowPoint (V3 _ _ z, _) = z

collide :: Brick -> Brick -> Bool
collide (V3 a b _, V3 c d _) (V3 e f _, V3 g h _)
  -- vertical bar. We check if our (x,y) coordinates fall somewhere inside the
  -- other brick.
  | a == c && b == d = e <= a && a <= g && f <= b && b <= h
  -- x-long bar. We first check if our y coordinate is within the span of the
  -- other brick. If that's the case, we check whether the x coordinates
  -- overlap, that is whether the smallest of the end coords is equal to or
  -- larger than the largest of the start coordinates.
  | b == d = f <= b && b <= h && (maxMinX <= minMaxX)
  -- y-long bar. We check if our x coordinate is within the span of the other
  -- brick. If that's the case, we check if there is an overlap in the y coordinates
  | otherwise = e <= a && a <= g && (maxMinY <= minMaxY)
  where
    maxMinX = max a e
    minMaxX = min c g
    maxMinY = max b f
    minMaxY = min d h

supports :: Brick -> Brick -> Bool
supports brick otherBrick =
  lowPoint otherBrick == highPoint brick + 1 && collide brick otherBrick

isSupported :: Brick -> Brick -> Bool
isSupported brick otherBrick =
  highPoint otherBrick == lowPoint brick - 1 && collide brick otherBrick

fall :: [Brick] -> Brick -> [Brick]
fall bricks brick@(V3 a b c, V3 d e f)
  -- path is blocked. We can't fall further
  | c == 1 || any (\b -> highPoint b == c - 1 && collide brick b) bricks =
    brick : bricks
  -- otherwise, fall.
  | otherwise = fall bricks fallen
  where
    fallen = (V3 a b (c - 1), V3 d e (f - 1))

part1 :: Bool -> String -> String
part1 _ input =
  show .
  length .
  filter (notElem 1) .
  map
    (\b ->
       map (\ob -> length . filter (isSupported ob) $ fallenStack) .
       filter (supports b) $
       fallenStack) $
  fallenStack
  where
    fallenStack =
      foldl fall [] .
      sortBy lowPointSort .
      map (toBricks . map toPos) .
      complexParser ["~"] ["[[:digit:]]+", "[[:digit:]]+"] $
      input

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
