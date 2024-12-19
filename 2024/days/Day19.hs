module Day19
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (second)
import           Data.Either          (fromRight, isRight)
import           Data.IntMap          as M (IntMap, fromList, keys, (!))
import           Data.List            as L (groupBy, null, sortBy)
import           Data.Maybe           (mapMaybe)
import           Data.Ord             (Down (..), comparing)
import           Data.Set             as S (Set, deleteFindMax, fromList,
                                            insert, map, member, null,
                                            singleton)
import           Data.Text            as T (Text, null, pack)
import           Helpers.Parsers.Text (Parser)
import           Text.Megaparsec      (eof, getInput, many, manyTill, observing,
                                       parse, sepBy, try, (<|>))
import           Text.Megaparsec.Char (eol, lowerChar, string)

type Towels = IntMap (Set String)

newtype Pattern =
  Pattern String
  deriving (Show, Eq)

instance Ord Pattern where
  compare (Pattern a) (Pattern b) =
    compare (length a) (length b) `mappend` compare a b

pSplitAt :: Int -> Pattern -> (String, Pattern)
pSplitAt x (Pattern p) = second Pattern . splitAt x $ p

pNull :: Pattern -> Bool
pNull (Pattern a) = L.null a

parseInput :: Parser (Towels, [Pattern])
parseInput = do
  towels <-
    M.fromList
      . fmap (\x -> (length . head $ x, S.fromList x))
      . groupBy (\a b -> length a == length b)
      . sortBy (comparing (Down . length))
      <$> many lowerChar `sepBy` string (pack ", ")
  eol
  eol
  patterns <- many (Pattern <$> manyTill lowerChar eol)
  return (towels, patterns)

-- Let's have a set of patterns. We take the
findPattern :: Towels -> Pattern -> Bool
findPattern towels = searchPat . singleton
  where
    searchPat :: Set Pattern -> Bool
    searchPat ps
      | S.null ps = False
      | any pNull samples = True
      | otherwise = searchPat ps''
      where
        ps'' = foldr insert ps' samples
        (toTest, ps') = deleteFindMax ps
        samples =
          fmap (snd . snd)
            . filter (\(a, (b, _)) -> b `member` a)
            . fmap (\x -> (towels ! x, pSplitAt x toTest))
            . keys
            $ towels

countPatterns :: Towels -> [Pattern] -> Int
countPatterns towels = length . filter (findPattern towels)

part1 :: Bool -> Text -> String
part1 _ =
  show
    . uncurry countPatterns
    . fromRight (error "parser failed")
    . parse parseInput "day19"

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
