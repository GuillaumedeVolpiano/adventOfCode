module Day23
  ( part1
  , part2
  ) where

import           Data.Hashable   (Hashable, hashWithSalt)
import           Data.List       as L (intersect, map, null, transpose)
import           Data.Maybe      (Maybe (Just, Nothing), catMaybes, isNothing)
import           Data.Set        as S (Set, delete, empty, fromList, insert,
                                       map, notMember, null)
import           Helpers.Parsers (characters)
import           Helpers.Search  (astarVal)

import           Debug.Trace

data Salamander
  = A
  | B
  | C
  | D
  deriving (Show, Eq, Ord)

type Burrow = (Hallway, [Room])

type Hallway = Set (Salamander, Int)

type Room = [Salamander]

instance Hashable Salamander where
  hashWithSalt = flip energy

hallPos :: [Int]
hallPos = [0, 1, 3, 5, 7, 9, 10]

isA :: Salamander -> Bool
isA A = True
isA _ = False

isB :: Salamander -> Bool
isB B = True
isB _ = False

isC :: Salamander -> Bool
isC C = True
isC _ = False

isD :: Salamander -> Bool
isD D = True
isD _ = False

setup :: [[String]] -> Burrow
setup s = (empty, rooms)
  where
    rooms = L.map (L.map toSalamander) . transpose $ s

edges :: Int -> Burrow -> [(Burrow, Int)]
edges depth burrow = allMoves
  where
    allMoves =
      moveRoomHallway depth burrow ++
      moveHallwayRoom depth burrow ++ moveRoomRoom depth burrow

moveRoomRoom :: Int -> Burrow -> [(Burrow, Int)]
moveRoomRoom depth (hallway, [a, b, c, d]) =
  catMaybes
    [ moveRoomRoomA depth hallway a (b : c : [d])
    , moveRoomRoomB depth hallway a b (c : [d])
    , moveRoomRoomC depth hallway (a : [b]) c [d]
    , moveRoomRoomD depth hallway (a : b : [c]) d
    ]

moveRoomRoomA :: Int -> Hallway -> Room -> [Room] -> Maybe (Burrow, Int)
moveRoomRoomA depth hallway a rooms
  | L.null a || all isA a = Nothing
  | isNothing moved = Nothing
  | otherwise =
    Just (newBurrow, energy (head a) (depth + 1 - length a) + hallEnergy)
  where
    moved = moveTo depth (head a, 2) (hallway, tail a : rooms)
    (Just (newBurrow, hallEnergy)) = moved

moveRoomRoomB :: Int -> Hallway -> Room -> Room -> [Room] -> Maybe (Burrow, Int)
moveRoomRoomB depth hallway a b rooms
  | L.null b || all isB b = Nothing
  | isNothing moved = Nothing
  | otherwise =
    Just (newBurrow, energy (head b) (depth + 1 - length b) + hallEnergy)
  where
    moved = moveTo depth (head b, 4) (hallway, a : tail b : rooms)
    (Just (newBurrow, hallEnergy)) = moved

moveRoomRoomC ::
     Int -> Hallway -> [Room] -> Room -> [Room] -> Maybe (Burrow, Int)
moveRoomRoomC depth hallway ab c rooms
  | L.null c || all isA c = Nothing
  | isNothing moved = Nothing
  | otherwise =
    Just (newBurrow, energy (head c) (depth + 1 - length c) + hallEnergy)
  where
    moved = moveTo depth (head c, 6) (hallway, ab ++ tail c : rooms)
    (Just (newBurrow, hallEnergy)) = moved

moveRoomRoomD :: Int -> Hallway -> [Room] -> Room -> Maybe (Burrow, Int)
moveRoomRoomD depth hallway rooms d
  | L.null d || all isD d = Nothing
  | isNothing moved = Nothing
  | otherwise =
    Just (newBurrow, energy (head d) (depth + 1 - length d) + hallEnergy)
  where
    moved = moveTo depth (head d, 8) (hallway, rooms ++ [tail d])
    (Just (newBurrow, hallEnergy)) = moved

moveRoomHallway :: Int -> Burrow -> [(Burrow, Int)]
moveRoomHallway depth (hallway, [a, b, c, d]) =
  moveA depth a hallway [b, c, d] ++
  moveB depth b hallway a [c, d] ++
  moveC depth c hallway [a, b] d ++ moveD depth d hallway [a, b, c]

moveHallwayRoom :: Int -> Burrow -> [(Burrow, Int)]
moveHallwayRoom depth burrow@(hallway, rooms) =
  catMaybes . foldl (\a b -> moveTo depth b burrow : a) [] $ hallway

moveTo :: Int -> (Salamander, Int) -> Burrow -> Maybe (Burrow, Int)
moveTo depth pair@(salamander, _)
  | isA salamander = moveToA depth pair
  | isB salamander = moveToB depth pair
  | isC salamander = moveToC depth pair
  | isD salamander = moveToD depth pair

moveToA :: Int -> (Salamander, Int) -> Burrow -> Maybe (Burrow, Int)
moveToA depth pair@(salamander, pos) (hallway, a:rooms)
  | length a == depth || not (all isA a) || not (reachable pos 2 hallSet) =
    Nothing
  | otherwise =
    Just
      ( (delete pair hallway, (salamander : a) : rooms)
      , energy salamander (abs (pos - 2) + depth - length a))
  where
    hallSet = S.map snd hallway

moveToB :: Int -> (Salamander, Int) -> Burrow -> Maybe (Burrow, Int)
moveToB depth pair@(salamander, pos) (hallway, a:b:rooms)
  | length b == depth || not (all isB b) || not (reachable pos 4 hallSet) =
    Nothing
  | otherwise =
    Just
      ( (delete pair hallway, a : (salamander : b) : rooms)
      , energy salamander (abs (pos - 4) + depth - length b))
  where
    hallSet = S.map snd hallway

moveToC :: Int -> (Salamander, Int) -> Burrow -> Maybe (Burrow, Int)
moveToC depth pair@(salamander, pos) (hallway, a:b:c:rooms)
  | length c == depth || not (all isC c) || not (reachable pos 6 hallSet) =
    Nothing
  | otherwise =
    Just
      ( (delete pair hallway, a : b : (salamander : c) : rooms)
      , energy salamander (abs (pos - 6) + depth - length c))
  where
    hallSet = S.map snd hallway

moveToD :: Int -> (Salamander, Int) -> Burrow -> Maybe (Burrow, Int)
moveToD depth pair@(salamander, pos) (hallway, a:b:c:[d])
  | length d == depth || not (all isD d) || not (reachable pos 8 hallSet) =
    Nothing
  | otherwise =
    Just
      ( (delete pair hallway, a : b : c : [salamander : d])
      , energy salamander (abs (pos - 8) + depth - length d))
  where
    hallSet = S.map snd hallway

moveA :: Int -> Room -> Hallway -> [Room] -> [(Burrow, Int)]
moveA depth room hallway rooms
  | L.null room || all isA room = []
  | otherwise = newHalls
  where
    availablePositions = canReach 2 $ S.map snd hallway
    salamander = head room
    newHalls =
      L.map
        (\p ->
           ( (S.insert (salamander, p) hallway, tail room : rooms)
           , energy salamander (abs (2 - p) + depth + 1 - length room)))
        availablePositions

moveB :: Int -> Room -> Hallway -> Room -> [Room] -> [(Burrow, Int)]
moveB depth room hallway a rooms
  | L.null room || all isB room = []
  | otherwise = newHalls
  where
    availablePositions = canReach 4 $ S.map snd hallway
    salamander = head room
    newHalls =
      L.map
        (\p ->
           ( (S.insert (salamander, p) hallway, a : tail room : rooms)
           , energy salamander (abs (4 - p) + depth + 1 - length room)))
        availablePositions

moveC :: Int -> Room -> Hallway -> [Room] -> Room -> [(Burrow, Int)]
moveC depth room hallway ab d
  | L.null room || all isC room = []
  | otherwise = newHalls
  where
    availablePositions = canReach 6 $ S.map snd hallway
    salamander = head room
    newHalls =
      L.map
        (\p ->
           ( (S.insert (salamander, p) hallway, ab ++ tail room : [d])
           , energy salamander (abs (6 - p) + depth + 1 - length room)))
        availablePositions

moveD :: Int -> Room -> Hallway -> [Room] -> [(Burrow, Int)]
moveD depth room hallway rooms
  | L.null room || all isD room = []
  | otherwise = newHalls
  where
    availablePositions = canReach 8 $ S.map snd hallway
    salamander = head room
    newHalls =
      L.map
        (\p ->
           ( (S.insert (salamander, p) hallway, rooms ++ [tail room])
           , energy salamander (abs (8 - p) + depth + 1 - length room)))
        availablePositions

energy :: Salamander -> Int -> Int
energy A k = k
energy B k = k * 10
energy C k = k * 100
energy D k = k * 1000

canReach :: Int -> Set Int -> [Int]
canReach pos hallSet =
  intersect hallPos $
  takeWhile (`notMember` hallSet) [(pos - 1),(pos - 2) .. 0] ++
  takeWhile (`notMember` hallSet) [(pos + 1) .. 10]

reachable :: Int -> Int -> Set Int -> Bool
reachable from to hallway = all (`notMember` hallway) [minFT + 1 .. maxFT - 1]
  where
    minFT = min from to
    maxFT = max from to

toSalamander :: String -> Salamander
toSalamander "A" = A
toSalamander "B" = B
toSalamander "C" = C
toSalamander "D" = D

heuristics :: Burrow -> Int
heuristics (hallway, [a, b, c, d]) =
  sum (S.map hallHeuristics hallway) + sum (L.map aHeuristics a) +
  sum (L.map bHeuristics b) +
  sum (L.map cHeuristics c) +
  sum (L.map dHeuristics d)
  where
    hallHeuristics (salamander, pos) =
      energy salamander (abs (pos - dest salamander) + 1)
    aHeuristics A          = 0
    aHeuristics salamander = energy salamander (dest salamander - 2)
    bHeuristiscs B = 0
    bHeuristics salamander = energy salamander (abs (dest salamander - 4))
    cHeuristics C          = 0
    cHeuristics salamander = energy salamander (abs (dest salamander - 6))
    dHeuristics D          = 0
    dHeuristics salamander = energy salamander (abs (dest salamander - 8))
    dest A = 2
    dest B = 4
    dest C = 6
    dest D = 8

isGoal :: Burrow -> Bool
isGoal (hallway, [a, b, c, d]) =
  S.null hallway && all isA a && all isB b && all isC c && all isD d

doSearch :: Int -> Burrow -> Int
doSearch depth burrow =
  astarVal burrow 0 isGoal (edges depth) heuristics (\a b -> b)

part1 :: Bool -> String -> String
part1 _ = show . doSearch 2 . setup . filter (not . L.null) . characters

part2 :: Bool -> String -> String
part2 _ =
  show .
  doSearch 4 .
  setup .
  (\[a, b] -> [a, ["D", "C", "B", "A"], ["D", "B", "A", "C"], b]) .
  filter (not . L.null) . characters
