import           General            (customPreciseTimeIt, retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Array.Unboxed (UArray, array, bounds, inRange, (!))
import           Data.Bifunctor     (second)
import           Data.List.Split    (chunksOf)
import           Data.Map           as M (Map, keys, lookup, member)
import           Data.Maybe         (Maybe (Just, Nothing), fromJust, isJust,
                                     mapMaybe)
import           Data.Set           (Set, fromList, notMember)
import           Debug.Trace
import           Linear.V3          (V3 (..))
import           Search             (dijkstraGoal)

data State =
  State
    { start :: Pos
    , goal  :: Pos
    , space :: Space
    }
  deriving (Show)

type Space = UArray Pos Bool

type Pos = V3 Int

parseInput :: [String] -> State
parseInput s = State st g sp
  where
    st = V3 xs (-1) 0
    g = V3 ys height 0
    xs = fst . head . filter (\x -> snd x == '.') . zip [(-1) ..] . head $ s
    height = length s - 2
    ys = fst . head . filter (\x -> snd x == '.') . zip [(-1) ..] . last $ s
    width = length (head s) - 2
    depth = lcm width height
    bnds = (V3 0 0 0, V3 (width - 1) (height - 1) (depth - 1))
    level0 =
      map (second fromJust) . filter (isJust . snd) $
      [ (V3 x y 0, blizFromChar $ s !! (y + 1) !! (x + 1))
      | x <- [0 .. width - 1]
      , y <- [0 .. height - 1]
      ]
    allLevels =
      fromList . concatMap (\a -> map (blow a) [0 .. depth - 1]) $ level0
    blow (a, b) c = V3 0 0 c + modulate (a + fmap (c *) b)
    modulate (V3 a b c) = V3 (mod a width) (mod b height) c
    sp =
      array
        (V3 0 0 0, V3 (width - 1) (height - 1) (depth - 1))
        [ (V3 x y z, notMember (V3 x y z) allLevels)
        | x <- [0 .. width - 1]
        , y <- [0 .. height - 1]
        , z <- [0 .. depth - 1]
        ]

blizFromChar :: Char -> Maybe Pos
blizFromChar c
  | c == '.' = Nothing
  | c == '>' = Just $ V3 1 0 0
  | c == '<' = Just $ V3 (-1) 0 0
  | c == '^' = Just $ V3 0 (-1) 0
  | c == 'v' = Just $ V3 0 1 0
  | otherwise = error ("character " ++ show c ++ " not mapped")

neighbours :: State -> Pos -> [(Pos, Int)]
neighbours (State st g space) pos =
  map (, 1) .
  filter (\a -> isGoal g a || isGoal st a || (inRange bds a && space ! a)) $
  toVisit
  where
    curPos = depthMod (pos + V3 0 0 1)
    bds@(_, V3 _ _ depth) = bounds space
    depthMod (V3 x y z) = V3 x y (mod z (depth + 1))
    toVisit =
      map (curPos +) [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 0]

path :: State -> (Pos, Int)
path state@(State st g@(V3 a b _) sp) = (pos, dist)
  where
    (_, V3 _ _ depth) = bounds sp
    dij = fst $ dijkstraGoal st 0 (neighbours state) (isGoal g)
    pos = head . filter (`member` dij) $ [V3 a b z | z <- [0 .. depth]]
    dist = fromJust . M.lookup pos $ dij

isGoal :: Pos -> Pos -> Bool
isGoal (V3 a b _) (V3 c d _) = a == c && b == d

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  customPreciseTimeIt "part 1 CPU time" 3 .
    print . snd . path . parseInput . lines $
    input
  customPreciseTimeIt "part 2 CPU time" 3 . print $ "solution 2"
