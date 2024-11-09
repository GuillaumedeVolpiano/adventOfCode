module Day15
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, ixmap)
import           Data.Function.HT   (compose2)
import           Data.List          as L (filter, foldl', map, minimumBy, null,
                                          sortBy, unfoldr)
import           Data.Maybe         (isJust, mapMaybe)
import           Data.Ord           (comparing)
import           Data.Sequence      as Sq (singleton)
import           Data.Set           as S (Set, delete, deleteFindMin, empty,
                                          filter, fromList, insert, map, member,
                                          notMember, null, singleton, toList,
                                          union)
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
  , pos  :: Pos
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

instance Eq Fighter where
  f1 == f2 = pos f1 == pos f2 && unit f1 == unit f2 && hp f1 == hp f2

-- Fighters are sorted in reading order
instance Ord Fighter where
  compare f1 f2 = readingOrder (pos f1) (pos f2)

type AP = Int

type HP = Int

type Cave = Set Pos

type Fighters = Set Fighter

type Ended = Bool

type State = (Fighters, Cave, Ended)

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

enemies :: Fighter -> Fighters -> Fighters
enemies f = S.filter (isEnemy f)

-- reading order : Top to bottom, left to right
readingOrder :: Pos -> Pos -> Ordering
readingOrder (V2 x0 y0) (V2 x1 y1) = compare y0 y1 `mappend` compare x0 x1

fightersCave :: AP -> UArray Pos Char -> State
fightersCave ap array = (fromList fighters, fromList cave, False)
  where
    cave = L.map fst . L.filter ((== '#') . snd) . assocs $ array
    fighters =
      L.map
        (\(p, c) ->
           if c == 'E'
             then Fighter Elf ap hitPoints p
             else Fighter Goblin attackPower hitPoints p)
        . L.filter (flip elem "EG" . snd)
        . assocs
        $ array

doTurn :: State -> Maybe (Fighters, State)
doTurn (fighters, cave, ended)
  | ended = Nothing
  | otherwise = Just (newFighters, (newFighters, cave, newEnded))
  where
    (newFighters, newEnded) = doMoves ((empty, fighters), ended)
    doMoves :: ((Fighters, Fighters), Bool) -> (Fighters, Bool)
    doMoves ((seen, toSee), hasEnded)
      -- No enemies left
      | hasEnded = (seen `union` toSee, hasEnded)
      -- All fighters have moved
      | S.null toSee = (seen, hasEnded)
      -- move the next fighter
      | otherwise = doMoves . move cave seen toSee $ ended

move :: Cave -> Fighters -> Fighters -> Bool -> ((Fighters, Fighters), Ended)
move cave seen toSee ended
  -- no enemies left. We're done
  | S.null enemyFighters = ((seen, toSee), True)
  -- check move then attack
  | otherwise = (attack cave newFighter seen remain, ended)
  where
    -- Current fighter is the first one, in reading order, that hasn't moved
    -- yet.
    (f, remain) = deleteFindMin toSee
    p = pos f
    -- we need to consider all the other remaining fighters, whether they have
    -- moved or not
    others = seen `union` remain
    enemyFighters = enemies f others
    -- Find all the walkable positions adjacent to enemy fighters. We need a
    -- list of all positions adjacent to be able to check if we are already
    -- adjacent to one.
    destList = (+) <$> (toList . S.map pos $ enemyFighters) <*> dirs
    inRange = L.filter walkable destList
    -- A position is walkable if it's not occupied by a fighter, including our
    -- current one, or part of the cave
    walkable x =
      x /= p && S.notMember x (S.map pos others) && S.notMember x cave
    -- Identify all walkable squares next to a given position
    neighbours pos = L.filter walkable . L.map (+ pos) $ dirs
    -- Targets are all inRange squares that can be reached from the fighter's
    -- position. We get pairs made of the position of the target and Maybe the length
    -- of the shortest path, and we filter out the ones that are Nothing. We
    -- need to build the targetList explicitely so we can check if it's empty.
    targetList =
      L.filter (isJust . snd)
        . L.map (\x -> (x, bfsSafeDist p neighbours (== x)))
        $ inRange
    -- The target is the closest to the fighter's position. If we have more than
    -- two at the same closest distance, we sort them by reading order of their
    -- position.
    target =
      fst . minimumBy (comparing snd `mappend` compose2 readingOrder fst)
        $ targetList
    -- Now we find the destination. For each walkable position next to our
    -- position, we calculate the length of the shortest path, if it exists, to
    -- our target. We know that at least one such shortest path exists because
    -- we have a target. Once again, if we have more than one destination with
    -- the same shortest distance to the target, we choose the first in reading
    -- order.
    dest =
      fst
        . minimumBy (comparing snd `mappend` compose2 readingOrder fst)
        . L.filter (isJust . snd)
        . L.map (\x -> (x, bfsSafeDist target neighbours (== x)))
        . L.filter walkable
        . L.map (p +)
        $ dirs
    newFighter
      -- if we don't have a target or are already in range, don't move. Checking
      -- if we are already in range first as we don't need to build a targetList
      -- if we are in range.
      | p `elem` destList || L.null targetList = f
      -- otherwise, make a step
      | otherwise = f {pos = dest}

attack :: Cave -> Fighter -> Fighters -> Fighters -> (Fighters, Fighters)
attack cave f moved toMove
  -- we can't reach an enemy. Just add our fighter to the list of those that
  -- have moved.
  | S.null targets = (insert f moved, toMove)
  -- otherwise we hurt a target, which can have moved or not, and just add our
  -- fighter to the moved set.
  | otherwise = (insert f newMoved, newToMove)
  where
    -- Enemies in range can have moved or not.
    enemyList = enemies f . union moved $ toMove
    adjacentPos = L.map (+ pos f) dirs
    -- targets are enemies that are in a position adjacent to us.
    targets = S.filter (flip elem adjacentPos . pos) enemyList
    -- Our target is the enemy with the lowest HPs. If two targets have the same
    -- lowest HPs, we break the tie by reading order.
    target =
      minimumBy (comparing hp `mappend` compose2 readingOrder pos) . toList
        $ targets
    movedTarget = target `member` moved
    -- If we have a target, then attack it with our attack power.
    newTarget = hurt (ap f) target
    newMoved
      -- the target has moved and is dead
      | movedTarget && isDead newTarget = delete target moved
      -- the target has moved and is still alive
      | movedTarget = insert newTarget . delete target $ moved
      -- otherwise, no changes
      | otherwise = moved
    newToMove
    -- the target hasn't moved yet, and is dead.
      | not movedTarget && isDead newTarget = delete target toMove
    -- the target hasn't moved yet and is still alive
      | not movedTarget = insert newTarget . delete target $ toMove
      -- otherwise, no changes
      | otherwise = toMove

score :: [Fighters] -> Int
score rounds = (length rounds - 1) * (foldr ((+) . hp) 0 . last $ rounds)

part1 :: Bool -> String -> String
part1 _ =
  show . score . unfoldr doTurn . fightersCave attackPower . arrayFromString

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
