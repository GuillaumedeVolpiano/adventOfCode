{-# LANGUAGE TemplateHaskell #-}

module Day8
  ( part1
  , part2
  ) where

import           Data.Bits                  (shiftL, (.&.))
import           Data.ByteString            (ByteString, pack)
import qualified Data.ByteString            as B (intercalate, length, null,
                                                  split, tail)
import           Data.ByteString.UTF8       (fromString)
import           Data.Char                  (ord)
import           Data.IntMap.Strict         (IntMap, fromList, insert, keys,
                                             (!))
import           Data.List                  (foldl', unfoldr)
import           Data.Word                  (Word8)
import           FlatParse.Basic            (char, eof, isLatinLetter,
                                             runParser, satisfy, some, string,
                                             takeLine, (<|>))
import           Helpers.Parsers.ByteString (alphaNum)
import           Helpers.Parsers.FlatParse  (Parser, extract)
import           Helpers.Search             (bfsDist)

type Tree = IntMap (Pos, Pos)

type Prune = IntMap Pos

type Instructions = String

type Pos = Int

type Step = Int

zzz = posify "ZZZ"

aaa = posify "AAA"

z = ord 'Z'

a = ord 'A'

posify :: String -> Int
posify = foldl' (\acc c -> ord c + shiftL acc 7) 0

parseInput :: Parser (Instructions, Tree)
parseInput = do
  instructions <- takeLine
  $(char '\n')
  tree <- parseTree
  pure (instructions, tree)

parseTree :: Parser Tree
parseTree = (eof >> pure mempty) <|> parseNode

parseNode :: Parser Tree
parseNode = do
  key <- posify <$> some (satisfy isLatinLetter)
  $(string " = (")
  left <- posify <$> some (satisfy isLatinLetter)
  $(string ", ")
  right <- posify <$> some (satisfy isLatinLetter)
  $(string ")\n")
  insert key (left, right) <$> parseTree

follow' :: Tree -> Pos -> Char -> Pos
follow' tree pos 'L' = fst $ tree ! pos
follow' tree pos 'R' = snd $ tree ! pos

pruneTree :: (Instructions, Tree) -> (Step, Prune)
pruneTree (instructions, tree) = (length instructions, prunedTree)
  where
    prunedTree =
      fromList . map (\a -> (a, foldl' (follow' tree) a instructions)) . keys
        $ tree

findZZZ :: Prune -> Pos -> Maybe (Pos, Pos)
findZZZ tree pos
  | pos == zzz = Nothing
  | otherwise = Just (tree ! pos, tree ! pos)

findZ :: Prune -> Pos -> Maybe (Pos, Pos)
findZ tree pos
  | pos .&. 127 == z = Nothing
  | otherwise = Just (tree ! pos, tree ! pos)

part1 :: Bool -> ByteString -> String
part1 _ input = show $ dist * step
  where
    (step, pruned) = pruneTree . extract . runParser parseInput $ input
    dist = length . unfoldr (findZZZ pruned) $ aaa

part2 :: Bool -> ByteString -> String
part2 _ input = show . (*) step . foldr1 lcm $ dists
  where
    (step, pruned) = pruneTree . extract . runParser parseInput $ input
    dists =
      map (length . unfoldr (findZ pruned)) . filter ((== a) . (.&. 127)) . keys
        $ pruned
