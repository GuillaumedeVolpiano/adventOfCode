{-# LANGUAGE TemplateHaskell #-}

module Day13
  ( part1
  , part2
  ) where

import           Data.Bifunctor            (bimap, first)
import           Data.Bits                 (shiftL, shiftR, (.&.))
import           Data.ByteString           (ByteString)
import           Data.Char                 (ord)
import           Data.IntMap.Strict        (IntMap, assocs, fromList,
                                            insertWith, (!))
import           Data.IntSet               (IntSet, insert, size, toList)
import           Data.List                 (permutations)
import           Data.Sequence             (Seq ((:<|), (:|>)))
import qualified Data.Sequence             as Sq (fromList, length)
import           FlatParse.Basic           (anyAsciiDecimalInt, eof,
                                            isLatinLetter, optional_, runParser,
                                            satisfy, skipSatisfy, some, string,
                                            switch, (<|>))
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)
import           Helpers.Search.Int        (travelingSalesman)

type Edges = IntMap Int

type Parser = F.Parser (IntSet, Edges)

parseInput :: Parser
parseInput = parseLine <|> (eof >> return (mempty, mempty))

parseLine :: Parser
parseLine = do
  firstNode <- ord . head <$> some (satisfy isLatinLetter)
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
  secondNode <- ord . head <$> some (satisfy isLatinLetter)
  $(string ".\n")
  bimap
    (insert firstNode . insert secondNode)
    (insertWith (+) (encodeRaw firstNode secondNode) (op gainLoss))
    <$> parseInput

encodeRaw :: Int -> Int -> Int
encodeRaw a b = u + shiftL l 7
  where
    l = min a b
    u = max a b

encode :: Int -> (Int, Int) -> Int
encode bitSize (a, b) = a + shiftL b bitSize

decode :: Int -> (Int, Int)
decode i = (i .&. 127, shiftR i 7)

simplify :: (IntSet, IntMap Int) -> (Int, IntMap Int)
simplify (nodes, rawEdges) = (numBits, edges)
  where
    numBits = ceiling . logBase 2 . fromIntegral . size $ nodes
    assocMap = fromList . flip zip [0 ..] . toList $ nodes
    edges =
      fromList
        . map
            (bimap
               (encode numBits . bimap (assocMap !) (assocMap !) . decode)
               negate)
        . assocs
        $ rawEdges

addMe :: (IntSet, IntMap Int) -> (IntSet, IntMap Int)
addMe (nodes, rawEdges) = (nodes', rawEdges')
  where
    nodes' = insert 0 nodes
    rawEdges' = foldr (flip (insertWith (+)) 0) rawEdges . toList $ nodes

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . negate
    . uncurry travelingSalesman
    . simplify
    . extract
    . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . negate
    . uncurry travelingSalesman
    . simplify
    . addMe
    . extract
    . runParser parseInput
