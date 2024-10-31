module Day15
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, ixmap)
import           Data.List          as L (filter, minimumBy, sortBy, unfoldr)
import           Data.Map           as M (Map, delete, elems, empty, filter,
                                          filterWithKey, fromList, insert, keys,
                                          lookup, member, notMember, (!))
import           Data.Maybe         (isJust, mapMaybe)
import           Data.Ord           (comparing)
import           Data.Sequence      as Sq (singleton)
import           Data.Set           as S (Set, fromList, notMember, singleton)
import           Helpers.Graph      (Pos, east, north, south, west)
import           Helpers.Parsers    (arrayFromString)
import           Helpers.Search     (bfsSafe, bfsSafeDist)
import           Linear.V2          (V2 (..))

import           Data.List.Split    (chunksOf)
import           Debug.Trace

data Fighter = Fighter
  { unit :: Unit
  , ap   :: AP
  , hp   :: HP
  } deriving (Show)

data Unit
  = Elf
  | Goblin
  deriving (Show, Eq)

type AP = Int

type HP = Int

type Cave = Set Pos

type Fighters = Map Pos Fighter

type Ended = Bool

type State = (Fighters, Cave, Ended)

hitPoints = 200

attackPower = 3

dirs = [north, west, east, south]

isGoblin :: Fighter -> Bool
isGoblin = (== Goblin) . unit

isElf :: Fighter -> Bool
isElf = not . isGoblin

isDead :: Fighter -> Bool
isDead = (< 1) . hp

hurt :: AP -> Fighter -> Fighter
hurt ap f = f {hp = hp f - ap}

enemies :: Pos -> Fighters -> Fighters
enemies pos fighters
  | isGoblin (fighters ! pos) = M.filter isElf fighters
  | otherwise = M.filter isGoblin fighters

readingOrder :: Pos -> Pos -> Ordering
readingOrder (V2 x0 y0) (V2 x1 y1) = compare y0 y1 `mappend` compare x0 x1

targetOrder :: Fighters -> Pos -> Pos -> Ordering
targetOrder f p1 p2 =
  compare (hp $ f ! p1) (hp $ f ! p2) `mappend` readingOrder p1 p2

fightersCave :: AP -> UArray Pos Char -> State
fightersCave ap array = (M.fromList fighters, S.fromList cave, False)
  where
    cave = map fst . L.filter ((== '#') . snd) . assocs $ array
    fighters =
      map
        (\(p, c) ->
           if c == 'E'
             then (p, Fighter Elf ap hitPoints)
             else (p, Fighter Goblin attackPower hitPoints))
        . L.filter (flip elem "EG" . snd)
        . assocs
        $ array

doTurn :: State -> Maybe (Fighters, State)
doTurn (fighters, cave, ended)
  | ended = Nothing
  | otherwise = Just (newFighters, (newFighters, cave, newEnded))
  where
    (newFighters, newEnded) =
      foldr (move cave) (fighters, ended) . sortBy (flip readingOrder) . keys
        $ fighters

render :: Fighters -> Cave -> String
render fighters cave =
  unlines . chunksOf (mY + 1) $ [rc x y | y <- [0 .. mY], x <- [0 .. mX]]
  where
    (V2 mX mY) = maximum cave
    rc x y
      | M.member (V2 x y) fighters && isElf (fighters ! V2 x y) = 'E'
      | M.member (V2 x y) fighters = 'G'
      | S.notMember (V2 x y) cave = '.'
      | otherwise = '#'

move :: Cave -> Pos -> (Fighters, Ended) -> (Fighters, Ended)
move cave p (fighters, ended)
  -- we're already done
  | ended = (fighters, True)
  -- fighter is dead
  | M.notMember p fighters = (fighters, ended)
  -- no enemies left. We're done
  | null enemyFighters = (fighters, True)
  -- we can reach a target. No need to move.
  | p `elem` destList = (attack cave p fighters, ended)
  -- no reachable target. Don't move.
  | null targetList = (fighters, ended)
  -- Let's move and then see if we can attack.
  | otherwise = (attack cave dest . insert dest f . delete p $ fighters, ended)
  where
    f = fighters ! p
    enemyFighters = sortBy readingOrder . keys . enemies p $ fighters
    destList = (+) <$> enemyFighters <*> dirs
    inRange = L.filter walkable destList
    targetList =
      L.filter (isJust . snd) . map (\x -> (x, bfsSafeDist p neighbours (== x)))
        $ inRange
    minDist = minimum . map snd $ targetList
    target =
      minimumBy readingOrder . map fst . L.filter ((== minDist) . snd)
        $ targetList
    dest =
      fst
        . minimumBy
            (\(x0, y0) (x1, y1) -> compare y0 y1 `mappend` readingOrder x0 x1)
        . L.filter (isJust . snd)
        . map (\x -> (x, bfsSafeDist x neighbours (== target)))
        . L.filter walkable . map (p +)
        $ dirs
    walkable :: Pos -> Bool
    walkable x = M.notMember x fighters && S.notMember x cave
    neighbours :: Pos -> [Pos]
    neighbours pos = L.filter walkable . map (+ pos) $ dirs

attack :: Cave -> Pos -> Fighters -> Fighters
attack cave p fighters
  | null targets = fighters
  | isDead newTarget = delete targetPos fighters
  | otherwise = insert targetPos newTarget fighters
  where
    f = fighters ! p
    targets = L.filter (`member` enemies p fighters) . map (p +) $ dirs
    targetPos = minimumBy (targetOrder fighters) targets
    target = fighters ! targetPos
    newTarget = hurt (ap f) target

score :: [Fighters] -> Int
score rounds = (length rounds - 1) * (foldr ((+) . hp) 0 . last $ rounds)

part1 :: Bool -> String -> String
part1 _ =
  show . score . unfoldr doTurn . fightersCave attackPower . arrayFromString

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
