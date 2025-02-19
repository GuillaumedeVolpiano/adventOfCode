module Day19
  ( part1
  , part2
  ) where

import           Data.Bifunctor             (second)
import           Data.ByteString            (ByteString, pack)
import qualified Data.ByteString            as B (length, null, splitAt)
import           Data.Either                (fromRight)
import           Data.IntMap                as M (IntMap, fromList, keys, (!))
import           Data.List                  as L (groupBy, length, null,
                                                  partition, sortBy)
import           Data.Maybe                 (mapMaybe)
import           Data.MultiSet              as MS (MultiSet, deleteMaxAll,
                                                   findMax, insertMany, null,
                                                   occur, singleton)
import           Data.Ord                   (Down (..), comparing)
import           Data.Set                   as S (Set, deleteFindMax, fromList,
                                                  insert, map, member, null,
                                                  singleton)
import           Data.Word8                 (_comma, _space)
import           Helpers.Parsers.ByteString (Parser)
import           Text.Megaparsec            (many, manyTill, parse, sepBy)
import           Text.Megaparsec.Byte       (eol, lowerChar, string)

type Towels = IntMap (Set ByteString)

newtype Pattern = Pattern
  { getPattern :: ByteString
  } deriving (Show, Eq)

instance Ord Pattern where
  compare (Pattern a) (Pattern b) =
    compare (B.length a) (B.length b) `mappend` compare a b

pSplitAt :: Int -> Pattern -> (ByteString, Pattern)
pSplitAt x (Pattern p) = second Pattern . B.splitAt x $ p

parseInput :: Parser (Towels, [Pattern])
parseInput = do
  towels <-
    M.fromList
      . fmap (\x -> (B.length . head $ x, S.fromList x))
      . groupBy (\a b -> B.length a == B.length b)
      . sortBy (comparing (Down . B.length))
      . fmap pack
      <$> many lowerChar `sepBy` string (pack [_comma, _space])
  eol
  eol
  patterns <- many (Pattern . pack <$> manyTill lowerChar eol)
  return (towels, patterns)

-- Let's have a set of patterns. We take the
findPattern :: Towels -> Pattern -> Bool
findPattern towels = searchPat . S.singleton
  where
    searchPat :: Set Pattern -> Bool
    searchPat ps
      | S.null ps = False
      | any (B.null . getPattern) samples' = True
      | otherwise = searchPat ps''
      where
        ps'' = foldr S.insert ps' samples'
        (toTest, ps') = S.deleteFindMax ps
        samples' = samples towels toTest

samples :: Towels -> Pattern -> [Pattern]
samples towels pat =
  fmap (snd . snd)
    . filter (\(a, (b, _)) -> b `S.member` a)
    . fmap (\x -> (towels ! x, pSplitAt x pat))
    . keys
    $ towels

findAllPatterns :: Towels -> Pattern -> Int
findAllPatterns towels = countPat . MS.singleton
  where
    countPat :: MultiSet Pattern -> Int
    countPat ps
      | MS.null ps = 0
      | otherwise = countNulls + countPat ps''
      where
        ps'' = foldr (`MS.insertMany` testCount) ps' nonNullSamples
        toTest = MS.findMax ps
        testCount = occur toTest ps
        ps' = deleteMaxAll ps
        (nullSamples, nonNullSamples) =
          partition (B.null . getPattern) . samples towels $ toTest
        countNulls = testCount * L.length nullSamples

countPatterns :: Towels -> [Pattern] -> Int
countPatterns towels = L.length . filter (findPattern towels)

countAllPatterns :: Towels -> [Pattern] -> Int
countAllPatterns towels = sum . fmap (findAllPatterns towels)

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . uncurry countPatterns
    . fromRight (error "parser failed")
    . parse parseInput "day19"

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . uncurry countAllPatterns
    . fromRight (error "parser failed")
    . parse parseInput "day19"
