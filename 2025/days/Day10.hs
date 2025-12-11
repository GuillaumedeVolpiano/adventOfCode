module Day10
  ( part1
  , part2
  , parseMachines
  ) where

import           Control.Monad            (void)
import           Data.Bifunctor           (bimap)
import           Data.Bits                (setBit, shiftL, xor, (.&.), shiftR)
import qualified Data.IntSet              as IS (insert, member, singleton)
import           Data.Sequence            (Seq (..), ViewL (EmptyL, (:<)))
import qualified Data.Sequence            as Sq (singleton, viewl, filter)
import           Data.Word                (Word8)
import           Data.Word8               (_braceleft, _braceright,
                                           _bracketleft, _bracketright, _comma,
                                           _lf, _numbersign, _parenleft,
                                           _parenright, _space)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (foldl', foldr')
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (deintercalate, eof, many,
                                                manyTill, one, satisfy)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS (insert, member)

data Machine = M {-# UNPACK #-} !Int !(Seq Int) ![Int]

data RBFS = Failure Int | Success Int Int | Prune deriving Show

parseMachines :: Parser Word8 IO (Int, Int)
parseMachines = P.manyTill parseMachine P.eof sumBoth

parseMachine :: Parser Word8 IO (Int, Int)
parseMachine = do
  void $ P.satisfy (==_bracketleft)
  i <- P.manyTill P.one (P.satisfy (==_bracketright)) toIndicator
  void $ P.satisfy (==_space)
  b <- P.manyTill parseButton (P.satisfy (==_braceleft)) toSeq
  j <- parseJoltage
  void $ P.satisfy (==_lf)
  pure $ bfsBoth $ M i b j

parseButton :: Parser Word8 IO Int
parseButton = do
  void $ P.satisfy (==_parenleft)
  b <- P.deintercalate (P.satisfy isDigit) (P.satisfy (==_comma)) toButton
  void $ P.satisfy (==_parenright)
  void $ P.satisfy (==_space)
  pure b

parseJoltage :: Parser Word8 IO [Int]
parseJoltage = do
  j <- P.deintercalate (P.many (P.satisfy isDigit) toNumber) (P.satisfy (==_comma)) eitherToList
  void $ P.satisfy (==_braceright)
  pure j

toIndicator :: Fold IO Word8 Int
toIndicator = flip F.foldr' 0 $ \w i -> (i `shiftL` 1) +
  if w ==_numbersign then 1 else 0

toSeq :: Fold IO Int (Seq Int)
toSeq = F.foldl' (:|>) mempty

toButton :: Fold IO (Either Word8 Word8) Int
toButton = flip F.foldl' 0 $ \b w ->
  case w of
    Left d  -> b `setBit` digit d
    Right _ -> b

toNumber :: Fold IO Word8 Int
toNumber = flip F.foldl' 0 $ \n w -> 10 * n + digit w

eitherToList :: Fold IO (Either Int Word8) [Int]
eitherToList = flip F.foldl' [] $ \ns v ->
  case v of
    Left n  -> n : ns
    Right _ -> ns

sumBoth :: Fold IO (Int, Int) (Int, Int)
sumBoth = F.foldl' (\(a, b) -> bimap (a +) (b +)) (0, 0)

bfsBoth :: Machine  -> (Int, Int)
bfsBoth (M i bs j) = (bfsIndicator i bs, result)
  where
    nToList 0 _ l = l
    nToList v n l = nToList (v - 1) (n `shiftR` 1) ((n .&. 1) : l)
    bs' = fmap (nToList (length j)) bs
    result = 0

bfsIndicator :: Int -> Seq Int -> Int
bfsIndicator i bs = bfs (Sq.singleton (0, 0)) (IS.singleton 0)
  where
    bfs toSee seen = if s == i then c else bfs toSee' seen'
      where
        (s, c, rest) = case Sq.viewl toSee of
                            EmptyL        -> undefined
                            ((v, a) :< r) -> (v, a, r)
        (toSee', seen') = foldr buildNext (rest, seen) bs
        buildNext b (t, st)
          | b' `IS.member` seen = (t, st)
          | otherwise = (t :|> (b', c + 1), IS.insert b' st)
          where
            b' = s `xor` b

tieredBFS :: Seq [Int] -> [Int] -> Int
tieredBFS bs js = undefined
  where
    (ns, ni, _) = foldr (\a (c, d, i) -> if a > c then (a, i, i + 1) else (c, d, i + 1)) ((0, 0, 0) :: (Int, Int, Int)) js
    usable = Sq.filter ((== 1) . (!! ni)) bs
    bfsPart sn seen = case Sq.viewl sn of
                        EmptyL -> []
                        ((n, c) :< rest) -> if n !! ni == ns
                                                then ((n, c):) $ bfsPart rest seen
                                                else  uncurry bfsPart .  foldr (updateSearch n c) (rest, seen) $ usable 
    updateSearch n c n' (nseq, set)
      | n'' `HS.member` set = (nseq, set)
      | otherwise = (nseq :|> (n'', c + 1), HS.insert n'' set)
      where
        n'' = zipWith (+) n n'


part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = S.parse parseMachines s >>= print . fst . either (\e -> error $ "Parser failed: " ++ show e) id

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = S.parse parseMachines s >>= print . snd . either (\e -> error $ "Parser failed: " ++ show e) id
