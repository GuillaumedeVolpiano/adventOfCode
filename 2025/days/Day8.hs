module Day8
  ( part1
  , part2
  , findCircuits
  , findLastCircuits
  ) where

import           Data.Hashable                     (Hashable, hashWithSalt)
import           Data.HashPSQ                      (HashPSQ)
import qualified Data.HashPSQ                      as PSQ (empty, insert,
                                                           minView)
import           Data.HashSet                      (HashSet)
import qualified Data.HashSet                      as HS (foldr, insert, member,
                                                          size, filter)
import qualified Data.List                         as L (sortBy)
import           Data.Word                         (Word8)
import           Data.Word8                        (_comma, _lf)
import           Helpers.General.Streamly          (digit, isDigit)
import qualified Streamly.Data.Fold                as F (foldl')
import           Streamly.Data.Fold                (Fold)
import qualified Streamly.Data.Parser              as P (eof, manyTill, satisfy)
import           Streamly.Data.Parser              (Parser)
import qualified Streamly.Data.Stream              as S (parse)
import           Streamly.Data.Stream              (Stream)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM (insert, foldrWithKey, adjust, (!), keys, keysSet)
import Data.Ord (comparing, Down(Down))

data Coords = C {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving (Eq, Ord, Show)

type Queue = HashPSQ (Coords, Coords) Int ()

type Circuit = HashMap Coords (HashSet Coords)

instance Hashable Coords where
  hashWithSalt s (C h w d) = hashWithSalt s (h, w, d)

distance :: Coords -> Coords -> Int
distance (C h w d) (C h' w' d') = ((h - h')^(2 :: Int)) + ((w - w')^(2 :: Int)) + ((d - d')^(2 :: Int))
{-# INLINE distance #-}

toInt :: Fold IO Word8 Int
toInt = F.foldl' (\a b -> digit b + 10 * a) 0

toQueue :: Fold IO Coords (Queue, Circuit)
toQueue = F.foldl' folder (PSQ.empty, mempty)
  where
    folder (queue, circuits) coords = (HM.foldrWithKey (addQueue coords) queue circuits, HM.insert coords mempty circuits)
    addQueue coords coords' _ = PSQ.insert (coords, coords') (distance coords coords') ()

parseLine :: Parser Word8 IO Coords
parseLine = do
  one <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_comma)) toInt
  two <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_comma)) toInt
  three <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_lf )) toInt
  pure $ C one two three

parseInput :: Parser Word8 IO (Queue, Circuit)
parseInput = P.manyTill parseLine P.eof toQueue

isConnected :: Coords -> Coords -> Circuit -> Bool
isConnected coords coords' circuit = bfsCircuit mempty [coords]
  where
    bfsCircuit _ [] = False
    bfsCircuit seen (x:xs)
      | coords' `HS.member` (circuit HM.! coords) = True
      | otherwise = bfsCircuit (HS.insert x seen) (HS.foldr (:) xs
          . HS.filter (not . flip HS.member seen) $ circuit HM.! x)


getCircuit :: Int -> Queue -> Circuit -> Circuit
getCircuit 0 _ circuit = circuit
getCircuit c queue circuit
    | isConnected coords coords' circuit = getCircuit c queue' circuit
    | otherwise = getCircuit (c - 1) queue' .
        HM.adjust (HS.insert coords) coords' . HM.adjust (HS.insert coords') coords $ circuit
  where
      mPairQueue = PSQ.minView queue
      ((coords, coords'), queue') = case mPairQueue of
                                      Nothing            -> error "empty queue"
                                      Just (k, _ , _, q) -> (k, q)

getAllCircuits :: Circuit -> [HashSet Coords]
getAllCircuits circuit = crawlCircuit coords mempty
  where
    coords = HM.keys circuit
    crawlCircuit [] sets = sets
    crawlCircuit (x:xs) sets = crawlCircuit (filter (not . flip HS.member nexts) xs) (nexts : sets)
      where
        nexts = crawlSubCircuit mempty [x]
        crawlSubCircuit seen [] = seen
        crawlSubCircuit seen (n:ns) = crawlSubCircuit (HS.insert n seen)
          (HS.foldr (:) ns . HS.filter (not . flip HS.member seen) $ circuit HM.! n)

connectAll :: Queue -> Circuit -> Int
connectAll queue circuit
  | allConnected = x * x'
  | otherwise = connectAll queue' circuit'
  where
    mPairQueue = PSQ.minView queue
    ((coords@(C x _ _), coords'@(C x' _ _)), queue') = case mPairQueue of
                                                          Nothing -> error "empty queue"
                                                          Just (k, _, _, q) -> (k, q)
    circuit' = HM.adjust (HS.insert coords) coords' . HM.adjust (HS.insert coords') coords $ circuit
    allConnected = fullCrawl mempty [coords]
    fullCrawl seen [] = seen == HM.keysSet circuit'
    fullCrawl seen (n:ns) = fullCrawl (HS.insert n seen) 
      (HS.foldr (:) ns . HS.filter (not . flip HS.member seen) $ circuit' HM.! n)

findLastCircuits :: Stream IO Word8 -> IO Int
findLastCircuits s = do
  (queue, circuit) <- either (error "parser failed") id <$> S.parse parseInput s
  pure $ connectAll queue circuit

findCircuits :: Bool -> Stream IO Word8 -> IO Int
findCircuits isTest s = do
    (queue, circuit) <- either (error "parser failed") id <$> S.parse parseInput s
    let ind = if isTest then 10 else 1000
        circuit' = getCircuit ind queue circuit
        allCircuits = getAllCircuits circuit'
    pure . product . take 3 .L.sortBy (comparing Down) . map HS.size $ allCircuits

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 isTest s = findCircuits isTest s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = findLastCircuits s >>= print
