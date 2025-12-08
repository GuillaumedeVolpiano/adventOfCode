module Day8
  ( part1
  , part2
  , parseInput
  , connectAll
  , getCircuit
  , prodThree
  , findCircuits
  , findLastCircuits
  , Queue
  , DisjointSets
  ) where

import           Control.DeepSeq          (NFData, rnf)
import           Data.Bifunctor           (first)
import           Data.Hashable            (Hashable, hashWithSalt)
import           Data.IntMap.Strict       (IntMap)
import qualified Data.IntMap.Strict       as IM (elems, filterWithKey, insert,
                                                 size, (!))
import qualified Data.List                as L (sortBy, uncons)
import           Data.Ord                 (Down (Down), comparing)
import           Data.Word                (Word8)
import           Data.Word8               (_comma, _lf)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (foldl')
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (eof, manyTill, satisfy)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)

data Coords = C {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord, Show)

type Queue = [(Int, Int, Int, Int, Int)]

data DisjointSets = DS  {
                          parent :: !(IntMap Int)
                        , size   :: !(IntMap Int)
                        }

instance NFData Coords where
  rnf (C x y z) = rnf x `seq` rnf y `seq` rnf z

instance Hashable Coords where
  hashWithSalt s (C h w d) = hashWithSalt s (h, w, d)

instance NFData DisjointSets where
  rnf (DS p s) = rnf p `seq` rnf s

instance Semigroup DisjointSets where
  (DS a b) <> (DS c d) = DS (a <> c) (b <> d)

instance Monoid DisjointSets where
  mempty = DS mempty mempty

mkSet :: Int -> DisjointSets -> DisjointSets
mkSet c (DS p s) = DS (IM.insert c c p) (IM.insert c 1 s)
{-# INLINE mkSet #-}

find :: Int -> DisjointSets -> (Int, DisjointSets)
find c ds
  | pc == c = (c, ds)
  | otherwise = (c', ds')
  where
    pc = parent ds IM.! c
    (c', DS p s) = find pc ds
    ds' = DS (IM.insert c c' p) s

union :: Int -> Int -> DisjointSets -> (DisjointSets, Int)
union c c' ds
  | x == y = (ds'', xs)
  | xs < ys = (DS (IM.insert x y p) (IM.insert y (xs + ys) s), xs + ys)
  | otherwise = (DS (IM.insert y x p) (IM.insert x (xs + ys) s), xs + ys)
  where
    (x, ds') = find c ds
    (y, ds''@(DS p s)) = find c' ds'
    xs = size ds'' IM.! x
    ys = size ds'' IM.! y

isRoot :: DisjointSets -> Int -> Bool
isRoot (DS p _) c = p IM.! c == c

distance :: Coords -> Coords -> Int
distance (C h w d) (C h' w' d') = ((h - h')^(2 :: Int)) + ((w - w')^(2 :: Int)) + ((d - d')^(2 :: Int))
{-# INLINE distance #-}

toInt :: Fold IO Word8 Int
toInt = F.foldl' (\a b -> digit b + 10 * a) 0

toQueue :: Fold IO Coords (Queue, DisjointSets)
toQueue = first (L.sortBy (\(a, _, _, _, _) (b, _, _, _, _) -> compare a b)) . (\(a, b, _, _) -> (a, b)) <$> F.foldl' folder ([], mempty, [], 0)
  where
    folder (queue, circuits, cs, p) coords = (foldr (addQueue coords p) queue cs
      , mkSet p circuits, (coords, p):cs, p + 1)
    addQueue coords@(C x _ _) p (coords'@(C x' _ _), p') = ((distance coords coords', p, p', x, x') :)

parseLine :: Parser Word8 IO Coords
parseLine = do
  one <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_comma)) toInt
  two <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_comma)) toInt
  three <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_lf )) toInt
  pure $ C one two three

parseInput :: Parser Word8 IO (Queue, DisjointSets)
parseInput = P.manyTill parseLine P.eof toQueue

getCircuit :: Int -> Queue -> DisjointSets -> DisjointSets
getCircuit 0 _ circuit = circuit
getCircuit c queue circuit = getCircuit (c - 1) queue' circuit'
  where
    mPairQueue = L.uncons queue
    ((_, coords, coords', _, _), queue') = case mPairQueue of
                                       Nothing -> error $ "empty queue, still needed " ++ show c ++ " connections."
                                       Just (v, q) -> (v, q)
    (circuit', _) = union coords coords' circuit

prodThree :: DisjointSets -> Int
prodThree ds@(DS _ s) = product . take 3 . L.sortBy (comparing Down) . IM.elems $ s'
  where
    s' = IM.filterWithKey (\k _ -> isRoot ds k) s


connectAll :: Int -> Queue -> DisjointSets -> Int
connectAll sz queue ds
  | ns == sz = x * x'
  | otherwise = connectAll sz queue' ds'
  where
    mPairQueue = L.uncons queue
    ((_, coords, coords', x, x'), queue') = case mPairQueue of
                                                          Nothing -> undefined
                                                          Just (k, q) -> (k, q)
    (ds', ns) = union coords coords' ds

findLastCircuits :: Stream IO Word8 -> IO Int
findLastCircuits s = do
  (queue, circuit) <- either (error "parser failed") id <$> S.parse parseInput s
  let sz = IM.size (parent circuit)
  pure $ connectAll sz queue circuit

findCircuits :: Bool -> Stream IO Word8 -> IO Int
findCircuits isTest s = do
    (queue, circuit) <- either (error "parser failed") id <$> S.parse parseInput s
    let ind = if isTest then 10 else 1000
        circuit' = getCircuit ind queue circuit
    pure $ prodThree circuit'

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 isTest s = findCircuits isTest s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = findLastCircuits s >>= print
