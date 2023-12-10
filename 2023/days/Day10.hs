module Day10
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as A (UArray, array, assocs, bounds,
                                          inRange, indices, (!))
import           Data.List          as L (filter, null)
import           Data.Maybe         (fromJust)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), fromList, null,
                                           (><))
import           Data.Set           as St (Set, empty, filter, fromList, insert,
                                           notMember, singleton, size, union)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

type Diagram = UArray Pos Pipe

type Pipe = Char

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

testSamplePart2 =
  "..........\n.S------7.\n.|F----7|.\n.||....||.\n.||....||.\n.|L-7F-J|.\n.|..||..|.\n.L--JL--J.\n.........."

pipes =
  [ ('|', [north, south])
  , ('-', [east, west])
  , ('L', [north, east])
  , ('J', [north, west])
  , ('7', [south, west])
  , ('F', [south, east])
  , ('.', [])
  , ('S', [north, south, east, west])
  ]

findStart :: Diagram -> Pos
findStart = fst . head . L.filter (\(a, b) -> b == 'S') . assocs

neighbours :: Diagram -> Pos -> [Pos]
neighbours diagram pos =
  L.filter (inRange $ bounds diagram) .
  map (pos +) . fromJust . lookup (diagram A.! pos) $
  pipes

explore :: Diagram -> Set Pos -> Pos -> Set Pos
explore diagram seen pos
  | L.null next = insert pos seen
  | otherwise = explore diagram (insert pos seen) . head $ next
  where
    next = L.filter (`notMember` seen) . neighbours diagram $ pos

startNeighbours :: Diagram -> Pos -> [Pos]
startNeighbours diagram pos =
  L.filter (elem pos . neighbours diagram) $ neighbours diagram pos

fill :: Diagram -> Seq Pos -> Set Pos -> Set Pos -> Set Pos
fill diagram toSee loop seen
  | Sq.null toSee = seen
  | otherwise = fill diagram newToSee loop newSeen
  where
    (pos :<| rest) = toSee
    toConsider =
      L.filter
        (\t ->
           inRange (bounds diagram) t && notMember t seen && notMember t loop) $
      map (pos +) [north, south, east, west]
    newToSee = rest >< Sq.fromList toConsider
    newSeen = union seen $ St.fromList toConsider

expandDiagram :: Diagram -> Diagram
expandDiagram diagram =
  array
    (V2 0 0, V2 (2 * width) (2 * height))
    [(V2 x y, expandVal x y) | x <- [0 .. 2 * width], y <- [0 .. 2 * height]]
  where
    b@(_, V2 width height) = bounds diagram
    expandVal x y
      | even x && even y = diagram ! V2 (div x 2) (div y 2)
      | even y &&
          inRange b (V2 (div (x - 1) 2) (div y 2)) &&
          inRange b (V2 (div (x + 1) 2) (div y 2)) &&
          elem (diagram ! V2 (div (x - 1) 2) (div y 2)) "F-LS" &&
          elem (diagram ! V2 (div (x + 1) 2) (div y 2)) "J-7S" = '-'
      | even x &&
          inRange b (V2 (div x 2) (div (y - 1) 2)) &&
          inRange b (V2 (div x 2) (div (y + 1) 2)) &&
          elem (diagram ! V2 (div x 2) (div (y - 1) 2)) "7F|S" &&
          elem (diagram ! V2 (div x 2) (div (y + 1) 2)) "JL|S" = '|'
      | otherwise = '.'

part1 :: Bool -> String -> String
part1 _ input =
  show .
  div
    (size . explore diagram (singleton start) . head . startNeighbours diagram $
     start) $
  2
  where
    diagram = arrayFromString input
    start = findStart diagram

part2 :: Bool -> String -> String
part2 test input = show $ totalSize - sizeLoop - sizeFilled
  where
    diagram
      | test = arrayFromString testSamplePart2
      | otherwise = arrayFromString input
    expanded = expandDiagram diagram
    (_, V2 mx my) = bounds expanded
    loop =
      explore expanded (singleton start) . head . startNeighbours expanded $
      start
    start = findStart expanded
    edgesSeq = Sq.fromList edges
    edgesSet = St.fromList edges
    edges =
      L.filter
        (`notMember` loop)
        [ V2 x y
        | x <- [0 .. mx]
        , y <- [0 .. my]
        , x == 0 || x == mx || y == 0 || y == my
        ]
    totalSize = length . indices $ diagram
    sizeLoop = St.size . St.filter (\(V2 x y) -> even x && even y) $ loop
    sizeFilled =
      St.size .
      St.filter (\(V2 x y) -> even x && even y) . fill expanded edgesSeq loop $
      edgesSet
