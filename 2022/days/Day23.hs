module Day23 (part1, part2) where
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Bifunctor     (second)
import           Data.List          as L (filter, groupBy, map, null, sortBy)
import           Data.List.Split    (chunksOf)
import           Data.Maybe         (catMaybes)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), filter, null,
                                           singleton)
import           Data.Set           as St (Set, delete, filter, fromList,
                                           insert, map, member, size, toList)
import           Linear.V2          (V2 (..))

data State =
  State Grove Directions
  deriving (Show)

type Grove = Set Pos

type Pos = V2 Int

type Directions = Seq (Pos, [Pos])

neighbours = [V2 x y | x <- [(-1) .. 1], y <- [(-1) .. 1], y /= 0 || x /= 0]

north = (V2 0 (-1), [V2 x (-1) | x <- [(-1) .. 1]])

south = (V2 0 1, [V2 x 1 | x <- [(-1) .. 1]])

west = (V2 (-1) 0, [V2 (-1) y | y <- [(-1) .. 1]])

east = (V2 1 0, [V2 1 y | y <- [(-1) .. 1]])

initialOrder = north :<| south :<| west :<| singleton east

parseInput :: [String] -> State
parseInput s =
  State
    (St.fromList
       [ V2 x y
       | x <- [0 .. length (head s) - 1]
       , y <- [0 .. length s - 1]
       , s !! y !! x == '#'
       ])
    initialOrder

doRound :: State -> State
doRound state@(State grove directions) =
  State
    (foldl (\a (b, c) -> insert c . delete b $ a) grove .
     concat .
     L.filter (\x -> length x == 1) .
     groupBy (\(_, a) (_, b) -> a == b) .
     sortBy (\(_, a) (_, b) -> compare a b) .
     catMaybes . toList . St.map (checkMoves state) . canMove $
     grove)
    (rotate directions)
  where
    rotate (s :<| ss) = ss :|> s

checkMoves :: State -> Pos -> Maybe (Pos, Pos)
checkMoves (State grove directions) elf = result
  where
    potMoves =
      Sq.filter
        (\(_, y) -> not (any (\x -> member (elf + x) grove) y))
        directions
    result
      | Sq.null potMoves = Nothing
      | otherwise = Just (elf, elf + fst move)
    (move :<| _) = potMoves

canMove :: Grove -> Grove
canMove grove = St.filter hasNeighbours grove
  where
    hasNeighbours elf = any (\x -> member (elf + x) grove) neighbours

printableGrove :: State -> String
printableGrove (State grove _) =
  unlines . chunksOf (width + 1) $
  [ if member (V2 x y) grove
    then '#'
    else '.'
  | y <- [miny .. maxy]
  , x <- [minx .. maxx]
  ]
  where
    width = maxx - minx
    maxx = maximum . toList . St.map (\(V2 a _) -> a) $ grove
    minx = minimum . toList . St.map (\(V2 a _) -> a) $ grove
    maxy = maximum . toList . St.map (\(V2 _ b) -> b) $ grove
    miny = minimum . toList . St.map (\(V2 _ b) -> b) $ grove

countEmpty :: State -> Int
countEmpty (State grove _) = (maxx - minx + 1) * (maxy - miny + 1) - size grove
  where
    xList = toList . St.map (\(V2 x _) -> x) $ grove
    yList = toList . St.map (\(V2 _ y) -> y) $ grove
    minx = minimum xList
    maxx = maximum xList
    miny = minimum yList
    maxy = maximum yList

rounds = iterate doRound . parseInput . lines

part1 :: Bool -> String -> String
part1 _ = show . countEmpty . last . take 11 . rounds

part2 :: Bool -> String -> String
part2 _ input = show $ 
    (length . takeWhile (\(State a _, State b _) -> a /= b) . zip r $
     tail r) +
    1
      where
        r = rounds input
