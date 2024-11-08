module Day15
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, ixmap)
import           Data.Function.HT   (compose2)
import           Data.List          as L (filter, foldl', minimumBy, sortBy,
                                          unfoldr)
import           Data.Map           as M (Map, delete, elems, filter,
                                          filterWithKey, foldlWithKey, fromList,
                                          insert, keys, lookup, member,
                                          notMember, (!))
import           Data.Maybe         (isJust, mapMaybe)
import           Data.Ord           (comparing)
import           Data.Sequence      as Sq (singleton)
import           Data.Set           as S (Set, empty, fromList, insert, member,
                                          notMember, singleton)
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
  }

data Unit
  = Elf
  | Goblin
  deriving (Eq)

instance Show Unit where
  show Elf = "E("
  show _   = "G("

instance Show Fighter where
  show f = show (unit f) ++ show (hp f) ++ ")"

type AP = Int

type HP = Int

type Cave = Set Pos

type Killed = Set Pos

type Fighters = Map Pos Fighter

type Ended = Bool

type State = (Int, Fighters, Cave, Ended)

hitPoints = 200

attackPower = 3

dirs = [north, west, east, south]

isGoblin :: Fighter -> Bool
isGoblin = (== Goblin) . unit

isElf :: Fighter -> Bool
isElf = not . isGoblin

isEnemy :: Fighter -> Fighter -> Bool
isEnemy f
  | isElf f = isGoblin
  | otherwise = isElf

isDead :: Fighter -> Bool
isDead = (< 1) . hp

hurt :: AP -> Fighter -> Fighter
hurt ap f = f {hp = hp f - ap}

enemies :: Pos -> Fighters -> Fighters
enemies pos fighters = M.filter (isEnemy (fighters ! pos)) fighters

readingOrder :: Pos -> Pos -> Ordering
readingOrder (V2 x0 y0) (V2 x1 y1) = compare y0 y1 `mappend` compare x0 x1

targetOrder :: Fighters -> Pos -> Pos -> Ordering
targetOrder f p1 p2 =
  compare (hp $ f ! p1) (hp $ f ! p2) `mappend` readingOrder p1 p2

fightersCave :: AP -> UArray Pos Char -> State
fightersCave ap array = (1, M.fromList fighters, S.fromList cave, False)
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
doTurn (turn, fighters, cave, ended)
  | ended = Nothing
  | otherwise =
    trace
      ("Round: " ++ show turn ++ "\n" ++ render newFighters cave ++ "\n")
      Just
      (newFighters, (turn + 1, newFighters, cave, newEnded))
  where
    ((newFighters, _), newEnded) =
      foldl' (move cave) ((fighters, empty), ended) . sortBy readingOrder . keys
        $ fighters

render :: Fighters -> Cave -> String
render fighters cave =
  unlines . insertFighters . chunksOf (mX + 1) $ [rc x y | y <- [0 .. mY], x <- [0 .. mX]]
  where
    (V2 mX mY) = maximum cave
    rc x y
      | M.member (V2 x y) fighters && isElf (fighters ! V2 x y) = 'E'
      | M.member (V2 x y) fighters = 'G'
      | S.notMember (V2 x y) cave = '.'
      | otherwise = '#'
    insertFighters l =
      foldlWithKey
        (\xs (V2 _ y) f -> insertFighterAt y (" " ++ show f ++ ",") xs)
        l
        fighters
    insertFighterAt :: Int -> String -> [String] -> [String]
    insertFighterAt y unit l = before ++ (there ++ unit) : after
      where
        (before, there:after) = splitAt y l

move ::
     Cave -> ((Fighters, Killed), Ended) -> Pos -> ((Fighters, Killed), Ended)
move cave ((fighters, killed), ended) p
  -- we're already done
  | ended = ((fighters, killed), True)
  -- fighter is dead
  | p `S.member` killed = ((fighters, killed), ended)
  -- no enemies left. We're done
  | null enemyFighters = trace (show p) ((fighters, killed), True)
  -- check move then attack
  | otherwise = trace (show p) (attack cave newPos newFighters killed, ended)
  where
    enemyFighters = sortBy readingOrder . keys . enemies p $ fighters
    destList = (+) <$> enemyFighters <*> dirs
    walkable x = M.notMember x fighters && S.notMember x cave
    inRange = L.filter walkable destList
    neighbours pos = L.filter walkable . map (+ pos) $ dirs
    targetList =
      L.filter (isJust . snd) . map (\x -> (x, bfsSafeDist p neighbours (== x)))
        $ inRange
    minDist = minimum . map snd $ targetList
    target =
      minimumBy readingOrder . map fst . L.filter ((== minDist) . snd)
        $ targetList
    dest =
      fst
        . minimumBy (comparing snd `mappend` compose2 readingOrder fst)
        . L.filter (isJust . snd)
        . map (\x -> (x, bfsSafeDist target neighbours (== x)))
        . L.filter walkable
        . map (p +)
        $ dirs
    f = fighters ! p
    (newPos, newFighters)
      -- if we don't have a target or are already in range, don't move
      | null targetList || p `elem` destList = (p, fighters)
      -- otherwise, make a step
      | otherwise = (dest, M.insert dest f . delete p $ fighters)

attack :: Cave -> Pos -> Fighters -> Killed -> (Fighters, Killed)
attack cave p fighters killed
  | null targets = (fighters, killed)
  | isDead newTarget = (delete targetPos fighters, S.insert targetPos killed)
  | otherwise = (M.insert targetPos newTarget fighters, killed)
  where
    targets = L.filter (`M.member` enemies p fighters) . map (p +) $ dirs
    targetPos = minimumBy (targetOrder fighters) targets
    target = fighters ! targetPos
    f = fighters ! p
    newTarget = hurt (ap f) target

score :: [Fighters] -> Int
score rounds = (length rounds - 1) * (foldr ((+) . hp) 0 . last $ rounds)

part1 :: Bool -> String -> String
part1 _ =
  show . score . unfoldr doTurn . fightersCave attackPower . arrayFromString

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
