module Day22
  ( part1
  , part2
  ) where

import           Data.List       as L (filter, map, sort)
import           Data.Set        as St (Set, delete, difference, empty, filter,
                                        fromList, insert, intersection, map,
                                        notMember, size)
import           Helpers.Parsers (complexParser)
import           Linear.V3       (V3 (..))

type Pos = V3 Int

type ID = Int

data Brick =
  Brick ID Pos Pos
  deriving (Show, Eq)

type Stack = Set Brick

instance Ord Brick where
  compare (Brick i (V3 a b c) _) (Brick j (V3 d e f) _)
    | a == d && b == e && c == f = compare i j
    | c == f && b == e = compare a d
    | c == f = compare b e
    | otherwise = compare c f

toPos :: [String] -> Pos
toPos [a, b, c] = V3 (read a) (read b) (read c)

toBricks :: (Int, [Pos]) -> Brick
toBricks (i, [x, y]) = Brick i x y

highPoint :: Brick -> Int
highPoint (Brick _ _ (V3 _ _ z)) = z

lowPoint :: Brick -> Int
lowPoint (Brick _ (V3 _ _ z) _) = z

collide :: Brick -> Brick -> Bool
collide (Brick _ (V3 a b _) (V3 c d _)) (Brick _ (V3 e f _) (V3 g h _))
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

supported :: Brick -> Brick -> Bool
supported brick otherBrick =
  lowPoint otherBrick == highPoint brick + 1 && collide brick otherBrick

isSupported :: Brick -> Brick -> Bool
isSupported brick otherBrick =
  highPoint otherBrick == lowPoint brick - 1 && collide brick otherBrick

fall :: Stack -> Brick -> Stack
fall bricks brick@(Brick iD (V3 a b c) (V3 d e f))
  -- path is blocked. We can't fall further
  | c == 1 || any (\b -> highPoint b == c - 1 && collide brick b) bricks =
    insert brick bricks
  -- otherwise, fall.
  | otherwise = fall bricks fallen
  where
    fallen = Brick iD (V3 a b (c - 1)) (V3 d e (f - 1))

disintegrate :: Stack -> Brick -> Stack
disintegrate bricks brick = foldl fall empty . delete brick $ bricks

supportingFilter :: Stack -> Brick -> Bool
supportingFilter stack brick =
  1 `elem`
  (foldl (\l ob -> (size . St.filter (isSupported ob) $ stack) : l) [] .
   St.filter (supported brick) $
   stack)

notSupporting :: Stack -> Stack
notSupporting stack = St.filter (not . supportingFilter stack) stack

supporting :: Stack -> Stack
supporting stack = St.filter (supportingFilter stack) stack

fallenStack :: String -> Stack
fallenStack =
  foldl fall empty .
  fromList .
  zipWith (curry toBricks) [1 ..] .
  L.map (L.map toPos) . complexParser ["~"] ["[[:digit:]]+", "[[:digit:]]+"]

countFall :: Stack -> Brick -> Int
countFall stack brick =
  size . difference (disintegrate stack brick) . delete brick $ stack

countAllFall :: Stack -> Stack -> Int
countAllFall fullStack = foldr ((+) . countFall fullStack) 0

part1 :: Bool -> String -> String
part1 _ = show . size . notSupporting . fallenStack

part2 :: Bool -> String -> String
part2 _ input = show . countAllFall fullStack $ isSupport
  where
    fullStack = fallenStack input
    isSupport = supporting fullStack
