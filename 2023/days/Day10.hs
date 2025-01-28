module Day10
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         as A (UArray, array, assocs, bounds,
                                                  inRange, indices, (!))
import           Data.ByteString            (ByteString)
import           Data.ByteString.UTF8       (fromString)
import           Data.List                  as L (filter, null, uncons)
import           Data.Maybe                 (fromJust, maybe, isNothing)
import           Data.Sequence              as Sq (Seq ((:<|), (:|>)), fromList,
                                                   null, (><))
import           Data.Set                   as St (Set, empty, filter, fromList,
                                                   insert, notMember, singleton,
                                                   size, union)
import           Data.Word                  (Word8)
import           Data.Word8                 (_7, _F, _J, _L, _S, _bar, _hyphen,
                                             _period)
import           Helpers.Parsers.ByteString (arrayFromByteString)
import           Linear.V2                  (V2 (..))

type Pos = V2 Int

type Diagram = UArray Pos Pipe

type Pipe = Word8

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

testSamplePart2 =
  fromString
    "..........\n.S------7.\n.|F----7|.\n.||....||.\n.||....||.\n.|L-7F-J|.\n.|..||..|.\n.L--JL--J.\n.........."

pipes =
  [ (_bar, [north, south])
  , (_hyphen, [east, west])
  , (_L, [north, east])
  , (_J, [north, west])
  , (_7, [south, west])
  , (_F, [south, east])
  , (_period, [])
  , (_S, [north, south, east, west])
  ]

findStart :: Diagram -> Pos
findStart = fst . maybe (error "Starting position not found") fst . uncons . L.filter (\(a, b) -> b == _S) . assocs

-- Which positions are accessible from a given position, based on the shape of
-- the loop at that position.
neighbours :: Diagram -> Pos -> [Pos]
neighbours diagram pos =
  L.filter (inRange $ bounds diagram)
    . map (pos +)
    . fromJust
    . lookup (diagram A.! pos)
    $ pipes

-- Follow the loop. From each point, you can access exactly two points, so go to
-- the one you haven't seen.
explore :: Diagram -> Set Pos -> Pos -> Set Pos
explore diagram seen pos = case next of
                              Nothing -> insert pos seen
                              Just (n, _) -> explore diagram (insert pos seen) n
  where
    next = uncons . L.filter (`notMember` seen) . neighbours diagram $ pos

-- We don't know the shape of the Start pipe, so we can't deduce the next steps
-- directly. We look at the four possible destinations and check which ones are
-- connected to Start.
startNeighbours :: Diagram -> Pos -> [Pos]
startNeighbours diagram pos =
  L.filter (elem pos . neighbours diagram) $ neighbours diagram pos

-- Basic reachability algorithm. Start from a reachable place and consider the
-- neighbours that have not been seen yet. They are reachable if they are in bound and not part of the pipe loop.
fill :: Diagram -> Seq Pos -> Set Pos -> Set Pos -> Set Pos
fill diagram toSee loop seen
  | Sq.null toSee = seen
  | otherwise = fill diagram newToSee loop newSeen
  where
    (pos :<| rest) = toSee
    toConsider =
      L.filter
        (\t ->
           inRange (bounds diagram) t && notMember t seen && notMember t loop)
        $ map (pos +) [north, south, east, west]
    newToSee = rest >< Sq.fromList toConsider
    newSeen = union seen $ St.fromList toConsider

-- Expand space so that we can squeeze between the pipes.
expandDiagram :: Diagram -> Diagram
expandDiagram diagram =
  array
    (V2 0 0, V2 (2 * width) (2 * height))
    [(V2 x y, expandVal x y) | x <- [0 .. 2 * width], y <- [0 .. 2 * height]]
  where
    b@(_, V2 width height) = bounds diagram
    expandVal x y
      | even x && even y = diagram ! V2 (div x 2) (div y 2)
      | even y
          && inRange b (V2 (div (x - 1) 2) (div y 2))
          && inRange b (V2 (div (x + 1) 2) (div y 2))
          && elem (diagram ! V2 (div (x - 1) 2) (div y 2)) [_F, _hyphen, _L, _S]
          && elem (diagram ! V2 (div (x + 1) 2) (div y 2)) [_J, _hyphen, _7, _S] =
        _hyphen
      | even x
          && inRange b (V2 (div x 2) (div (y - 1) 2))
          && inRange b (V2 (div x 2) (div (y + 1) 2))
          && elem (diagram ! V2 (div x 2) (div (y - 1) 2)) [_7, _F, _bar, _S]
          && elem (diagram ! V2 (div x 2) (div (y + 1) 2)) [_J, _L, _bar, _S] =
        _bar
      | otherwise = _period

part1 :: Bool -> ByteString -> String
part1 _ input =
  show
    . div
        (size
           . explore diagram (singleton start)
           . maybe (error "no neighbours found") fst . uncons
           . startNeighbours diagram
           $ start)
    $ 2
  where
    diagram = arrayFromByteString input
    start = findStart diagram

part2 :: Bool -> ByteString -> String
part2 test input = show $ totalSize - sizeLoop - sizeFilled
  where
    diagram
      | test = arrayFromByteString testSamplePart2
      | otherwise = arrayFromByteString input
    expanded = expandDiagram diagram
    (_, V2 mx my) = bounds expanded
    loop =
      explore expanded (singleton start) . maybe (error "no neighbours found") fst . uncons  . startNeighbours expanded
        $ start
    start = findStart expanded
    edgesSeq = Sq.fromList edges
    edgesSet = St.fromList edges
    -- the edges are reachable if they are not part of the loop, so they make a
    -- good starting point.
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
      St.size
        . St.filter (\(V2 x y) -> even x && even y)
        . fill expanded edgesSeq loop
        $ edgesSet
