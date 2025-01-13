{-# LANGUAGE TemplateHaskell #-}

module Day13
  ( part1
  , part2
  ) where

import           Data.Bifunctor            (bimap)
import           Data.ByteString           (ByteString)
import           Data.Char                 (ord)
import           Data.IntSet               (IntSet, size, toList)
import qualified Data.IntSet               as S (insert)
import           Data.List                 (permutations)
import           Data.Map                  (Map, assocs, filterWithKey, member,
                                            (!))
import qualified Data.Map                  as M (insert)
import           Data.Sequence             (Seq ((:<|), (:|>)))
import qualified Data.Sequence             as Sq (fromList, length)
import           FlatParse.Basic           (anyAsciiDecimalInt, eof,
                                            isLatinLetter, optional_, runParser,
                                            satisfy, skipSatisfy, some, string,
                                            switch, (<|>))
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

import           Debug.Trace

type Edges = Map (Int, Int) Int

type Parser = F.Parser (IntSet, Edges)

parseInput :: Parser
parseInput = parseLine <|> (eof >> return (mempty, mempty))

parseLine :: Parser
parseLine = do
  first <- encode <$> some (satisfy isLatinLetter)
  $(string " would ")
  op <-
    $(switch
        [|case _ of
            "gain " -> return id
            "lose " -> return negate|])
  gainLoss <- anyAsciiDecimalInt
  $(string " happiness unit")
  optional_ . skipSatisfy $ (== 's')
  $(string " by sitting next to ")
  second <- encode <$> some (satisfy isLatinLetter)
  $(string ".\n")
  bimap (S.insert first . S.insert second) (M.insert (first, second) gainLoss)
    <$> parseInput

encode :: String -> Int
encode = (+ negate 65) . ord . head

sittingEm :: (IntSet, Edges) -> Int
sittingEm (guests, edges) =
  maximum
    . map closeCircle
    . assocs
    . filterWithKey (\k _ -> length k == size guests - 1)
    . foldr arrangementsVal mempty
    $ arrangements
  where
    (honor:rest) = toList guests
    arrangements = map Sq.fromList . permutations $ rest
    arrangementsVal arr arrs
      | Sq.length arr == 1 = M.insert arr 0 arrs
      | rs `member` arrs = M.insert arr dist arrs
      | rs' `member` arrs = M.insert arr dist' arrs
      | otherwise = M.insert arr dist'' arrs'
      where
        (a :<| rs@(r :<| _)) = arr
        (rs'@(_ :|> r') :|> a') = arr
        dist = edges ! (a, r) + arrs ! rs
        dist' = edges ! (r', a') + arrs ! rs'
        arrs' = arrangementsVal rs arrs
        dist'' = edges ! (a, r) + arrs' ! rs
    closeCircle ((guest :<| _) :|> guest', happiness) =
      happiness + edges ! (guest', honor) + edges ! (honor, guest)

part1 :: Bool -> ByteString -> String
part1 _ = show . sittingEm . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ _ = "Part 2"
