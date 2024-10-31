module Day15
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, ixmap)
import           Data.List          as L (filter, minimumBy, unfoldr)
import           Data.Map           as M (Map, delete, elems, empty, filter,
                                          foldlWithKey, fromList, insert, keys,
                                          lookup, member, notMember, (!))
import           Data.Maybe         (isJust, mapMaybe)
import           Data.Ord           (comparing)
import           Data.Sequence      as Sq (singleton)
import           Data.Set           as S (Set, fromList, notMember, singleton)
import           Helpers.Graph      (Pos, east, north, south, west)
import           Helpers.Parsers    (arrayFromString)
import           Helpers.Search     (bfsSafe, bfsSafeDist)
import           Linear.V2          (V2 (..))

import           Debug.Trace

data Fighter
  = Goblin AP HP
  | Elf AP HP
  deriving (Show)

type AP = Int

type HP = Int

type Cave = Set Pos

type Fighters = Map Pos Fighter

type Ended = Bool

type State = (Fighters, Cave, Ended)

hitPoints = 200

attackPower = 3

-- applying the (V2 x y -> V2 y x) symmetry to [north, west, east, south]
dirs = [west, north, south, east]

isGoblin :: Fighter -> Bool
isGoblin (Goblin _ _) = True
isGoblin _            = False

isElf :: Fighter -> Bool
isElf = not . isGoblin

isDead :: Fighter -> Bool
isDead (Goblin _ hp) = hp < 1
isDead (Elf _ hp)    = hp < 1

hurt :: AP -> Fighter -> Fighter
hurt ap (Goblin a h) = Goblin a (h - ap)
hurt ap (Elf a h)    = Elf a (h - ap)

getAP :: Fighter -> AP
getAP (Goblin ap _) = ap
getAP (Elf ap _)    = ap

getHP :: Fighter -> HP
getHP (Goblin _ hp) = hp
getHP (Elf _ hp)    = hp

enemy :: Fighter -> Fighter -> Bool
enemy f
  | isGoblin f = isElf
  | otherwise = isGoblin

fightersCave :: AP -> UArray Pos Char -> State
fightersCave ap array = (M.fromList fighters, S.fromList cave, False)
  where
    -- we apply a (V2 x y -> V2 y x) symmetry so that vectors get sorted by x
    -- first, then by y, as per the reading order.
    cave =
      map ((\(V2 x y) -> V2 y x) . fst) . L.filter ((== '#') . snd) . assocs
        $ array
    fighters =
      map
        (\(V2 x y, c) ->
           if c == 'E'
             then (V2 y x, Elf ap hitPoints)
             else (V2 y x, Goblin attackPower hitPoints))
        . L.filter (flip elem "EG" . snd)
        . assocs
        $ array

doTurn :: State -> Maybe (Fighters, State)
doTurn (fighters, cave, ended)
  | ended = Nothing
  | otherwise = Just (newFighters, (newFighters, cave, newEnded))
  where
    (newFighters, newEnded) =
      foldlWithKey (move cave) (fighters, ended) fighters

move :: Cave -> (Fighters, Ended) -> Pos -> Fighter -> (Fighters, Ended)
move cave (fighters, ended) p f
  | ended || null enemies = (fighters, True)
  | M.notMember p fighters = (fighters, False)
  | any
      ((\x -> M.member x fighters && (enemy f . (!) fighters $ x)) . (p +))
      dirs = (attack cave p f fighters, False)
  | null targetList = (fighters, False)
  | otherwise =
    (attack cave dest f . insert dest f . delete p $ fighters, False)
  where
    enemies = keys . M.filter (enemy f) $ fighters
    destList = (+) <$> dirs <*> enemies
    inRange = L.filter walkable destList
    targetList =
      mapMaybe
        (\x ->
           bfsSafe (Sq.singleton p) (S.singleton p) M.empty neighbours (== x))
        inRange
    target =
      head
        . minimumBy
            (\a b ->
               (if length a == length b
                  then compare (head a) (head b)
                  else compare (length a) (length b)))
        $ targetList
    dest =
      fst
        . minimumBy
            (\(x0, y0) (x1, y1) ->
               if y0 == y1
                 then compare x0 x1
                 else compare y0 y1)
        . L.filter (isJust . snd)
        . map ((\x -> (x, bfsSafeDist x neighbours (== target))) . (p +))
        . L.filter (walkable . (p +))
        $ dirs
    walkable :: Pos -> Bool
    walkable x = M.notMember x fighters && S.notMember x cave
    neighbours :: Pos -> [Pos]
    neighbours pos = L.filter walkable . map (+ pos) $ dirs

attack :: Cave -> Pos -> Fighter -> Fighters -> Fighters
attack cave p f fighters
  | null targets = fighters
  | isDead newTarget = delete targetPos fighters
  | otherwise = insert targetPos newTarget fighters
  where
    targets =
      L.filter (\x -> M.member x fighters && (enemy f . (!) fighters $ x))
        . map (p +)
        $ dirs
    targetPos = minimumBy (comparing (getHP . (!) fighters)) targets
    target = fighters ! targetPos
    newTarget = hurt (getAP f) target

score :: [Fighters] -> Int
score rounds = (length rounds - 1) * (foldr ((+) . getHP) 0 . last $ rounds)

part1 :: Bool -> String -> String
part1 _ =
  show . score . unfoldr doTurn . fightersCave attackPower . arrayFromString

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
