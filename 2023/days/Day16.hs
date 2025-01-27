module Day16
  ( part1
  , part2
  ) where

import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (Reader, ask, runReader)
import           Data.Bits              (shiftL)
import           Data.ByteString        (ByteString, split, unpack)
import           Data.Char              (chr)
import           Data.Foldable          (foldrM)
import           Data.Hashable          (Hashable, hashWithSalt)
import qualified Data.IntMap.Strict     as M (IntMap, fromList, insert, member,
                                              (!))
import           Data.IntSet            (union)
import qualified Data.IntSet            as St (IntSet, fromList, insert, member,
                                               singleton, size)
import qualified Data.Map.Strict        as MS (Map, fromList, (!))
import           Data.Massiv.Array      (Array, Comp (Seq), Ix2 (..), P,
                                         Sz (..), fromLists', ifoldrS, index')
import qualified Data.Massiv.Array      as A (size, (!))
import           Data.Maybe             (fromJust, isNothing)
import           Data.Word              (Word8)
import           Data.Word8             (_backslash, _bar, _hyphen, _period,
                                         _slash)
import qualified Streamly.Data.Fold     as S (maximum)
import qualified Streamly.Data.Stream   as S (fold, mapM)
import           Streamly.Data.Stream   (Stream, enumerateFromTo, unfold,
                                         unfoldMany)
import           Streamly.Data.Unfold   (Unfold, many, unfoldr)

type Map = M.IntMap

type Graph = Map (Pos, Word8, Set) -- Map Beam (Pos, Word8, Set Pos)

type Dir = Ix2

type Pos = Ix2

type Cave = Array P Pos Word8

type BoundedCave = (Cave, Sz Pos)

type Set = St.IntSet

data Beam =
  Beam Pos Pos
  deriving (Show, Eq, Ord)

instance Hashable Beam where
  hashWithSalt salt (Beam a b) = hashWithSalt salt (a, b)

instance Hashable Ix2 where
  hashWithSalt salt (y :. x) = hashWithSalt salt (y, x)

class Intifiable a where
  intify :: a -> Int

instance Intifiable Ix2 where
  intify (y :. x) = shiftL y 7 + x

instance Intifiable Beam where
  intify (Beam pos dir) = shiftL (intify dir) 14 + intify pos

north = (-1) :. 0

south = 1 :. 0

east = 0 :. 1

west = 0 :. (-1)

origin = 0 :. 0

startPos = Beam origin east

reflections =
  MS.fromList
    [ ((north, _slash), east)
    , ((north, _backslash), west)
    , ((south, _slash), west)
    , ((south, _backslash), east)
    , ((west, _slash), south)
    , ((west, _backslash), north)
    , ((east, _slash), north)
    , ((east, _backslash), south)
    ]

nodes :: Reader BoundedCave [Pos]
nodes =
  ask >>= \(cave, Sz2 my mx) ->
    pure
      . ifoldrS
          (\i@(iy :. ix) e acc ->
             if iy == 0 || iy == my || ix == 0 || ix == mx || e /= _period
               then i : acc
               else acc)
          []
      $ cave

findNexts :: Pos -> Graph -> Reader BoundedCave Graph
findNexts pos graph = foldrM (findNext pos) graph [north, south, east, west]

findNext :: Pos -> Dir -> Graph -> Reader BoundedCave Graph
findNext pos@(y :. x) dir graph = do
  let beam = Beam pos dir
  ended <- atEnd beam
  let result
        | ended = pure graph
        | otherwise =
          crawl (St.singleton (intify pos)) (Beam (pos + dir) dir) >>= \next ->
            pure . M.insert (intify beam) next $ graph
  result

crawl :: Set -> Beam -> Reader BoundedCave (Pos, Word8, Set)
crawl crossed beam@(Beam pos dir) = do
  c <- ask
  ended <- atEnd beam
  let crossed' = St.insert (intify pos) crossed
      (cave, Sz2 my mx) = c
      beam' = Beam (pos + dir) dir
      modifier = cave A.! pos
  if ended || modifier `elem` [_hyphen, _bar, _slash, _backslash]
    then pure (pos, modifier, crossed')
    else crawl crossed' beam'

atEnd :: Beam -> Reader BoundedCave Bool
atEnd (Beam pos@(y :. x) dir) =
  ask >>= \(cave, Sz2 my mx) ->
    let c = cave A.! pos
     in pure
          $ y == 0 && dir == north
              || x == 0 && dir == west
              || y == my && dir == south
              || x == mx && dir == east

buildGraph :: Reader BoundedCave Graph
buildGraph = do
  nodeSet <- nodes
  foldrM findNexts mempty nodeSet

unfoldX :: (Monad m) => Int -> Unfold m Int (Int, [Dir])
unfoldX mx =
  unfoldr $ \x ->
    if x > mx
      then Nothing
      else Just ((x, [north, south, east, west]), x + 1)

unfoldDir :: (Monad m) => Int -> Graph -> Unfold m (Int, [Dir]) Beam
unfoldDir mx graph =
  unfoldr $ \(x, ds) ->
    let (d:ds') = ds
     in if null ds
          then Nothing
          else Just (beamify mx x d, (x, ds'))

beamify :: Int -> Int -> Dir -> Beam
beamify mx x d
  | d == south = Beam (0 :. x) d
  | d == north = Beam (mx :. x) d
  | d == east = Beam (x :. 0) d
  | d == west = Beam (x :. mx) d

dfs :: Graph -> [Beam] -> Set -> Set -> Int
dfs nexts toSee seen seenPos
  | null toSee = St.size seenPos
  | otherwise = dfs nexts toSee' seen' seenPos'
  where
    (beam@(Beam _ nextDir):rest) = toSee
    (nextPos, modifier, newCrossed) = nexts M.! intify beam
    toConsider = pivot (Beam nextPos nextDir) modifier
    nextBeams =
      filter
        (\x -> not (St.member (intify x) seen) && M.member (intify x) nexts)
        toConsider
    toSee' = nextBeams ++ rest
    seen' = foldr (St.insert . intify) seen toConsider
    seenPos' = newCrossed `union` seenPos

pivot :: Beam -> Word8 -> [Beam]
pivot beam@(Beam pos dir) modifier
  | modifier `elem` [_slash, _backslash] =
    pure . Beam pos $ reflections MS.! (dir, modifier)
  | modifier == _bar && dir `elem` [east, west] = map (Beam pos) [north, south]
  | modifier == _hyphen && dir `elem` [north, south] =
    map (Beam pos) [east, west]
  | otherwise = pure beam

boundCave :: Cave -> BoundedCave
boundCave cave = (cave, (\(Sz ix) -> Sz (ix - 1)) . A.size $ cave)

inRange :: Beam -> Reader BoundedCave Bool
inRange (Beam (y :. x) _) =
  ask >>= \(_, Sz2 my mx) -> pure $ y >= 0 && y <= my && x >= 0 && x <= mx

bounceRay :: Beam -> Reader BoundedCave Int
bounceRay beam = do
  graph <- buildGraph
  dfsify graph beam

bounceRays :: Reader BoundedCave Int
bounceRays = do
  c <- ask
  graph <- buildGraph
  let (_, Sz2 my mx) = c
      beams =
        S.fold S.maximum
          . S.mapM (dfsify graph)
          . unfold (many (unfoldDir mx graph) (unfoldX mx))
          $ 0
  fromJust <$> beams

dfsify :: Graph -> Beam -> Reader BoundedCave Int
dfsify graph beam@(Beam pos dir) = do
  (cave, _) <- ask
  let modifier = cave A.! pos
      beams = pivot beam modifier
  pure
    $ dfs
        graph
        beams
        (St.fromList . map intify $ beams)
        (St.singleton (intify pos))

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . runReader (bounceRay startPos)
    . boundCave
    . fromLists' Seq
    . map unpack
    . init
    . split 10

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . runReader bounceRays
    . boundCave
    . fromLists' Seq
    . map unpack
    . init
    . split 10
