{-# LANGUAGE TemplateHaskell #-}

module Day14
  ( part1
  , part2
  ) where

import           Data.Bifunctor            (first, second)
import           Data.Bits                 (shiftL, shiftR, (.&.))
import           Data.ByteString           (ByteString)
import           Data.IntSet               (IntSet, insert, notMember, union)
import qualified Data.IntSet               as S (filter, foldr, map)
import           Data.List                 (foldl')
import           Data.Maybe                (fromJust)
import           Data.Sequence             as Sq (Seq ((:<|)), drop, iterateN,
                                                  length, take, takeWhileL,
                                                  (!?))
import           Data.Vector.Unboxed       (Vector, generate)
import qualified Data.Vector.Unboxed       as V (fromList, (!))
import           FlatParse.Stateful        (eof, get, modify, put, runParser,
                                            switch, (<|>))
import           Helpers.Parsers.FlatParse (ParserS, extractS)
import           Helpers.Search            (findPattern)

type Rocks = IntSet

type Fixed = Vector Bool

north = -128

south = 128

east = 1

west = -1

numCycles = 1000000000

parseInput :: ParserS () (Rocks, IntSet)
parseInput =
  $(switch
      [|case _ of
          "."  -> modify (+ 1) >> parseInput
          "O"  -> get >>= \x -> put (x + 1) >> first (insert x) <$> parseInput
          "#"  -> get >>= \x -> put (x + 1) >> second (insert x) <$> parseInput
          "\n" -> modify ((+ 128) . (.&. 16256)) >> parseInput|])
    <|> (eof >> pure (mempty, mempty))

makeVector :: IntSet -> Fixed
makeVector set = generate (2 ^ 14) (`notMember` set)

move :: Fixed -> Int -> Rocks -> Rocks
move fixed dir rocks = allMoved
  where
    my = shiftL 99 7
    mx = 99
    order
      | dir == north = [my,my - 128 .. my .&. 127]
      | dir == south = [0,128 .. my]
      | dir == west = [mx,mx - 1 .. 0]
      | dir == east = [0 .. mx]
    coord
      | dir `elem` [north, south] = flip shiftR 7
      | otherwise = (.&. 127)
    allMoved = foldr displaceByRow mempty order
    displaceByRow x moved = (S.map (fullMove moved) . atX $ x) `union` moved
    atX x = S.filter (\p -> coord p == coord x) rocks
    canMove moved p =
      py <= 99
        && py >= 0
        && px <= 99
        && px >= 0
        && mp `notMember` moved
        && fixed V.! mp
      where
        mp = p + dir
        py = shiftR mp 7
        px = mp .&. 127
    fullMove moved x
      | canMove moved x = fullMove moved (x + dir)
      | otherwise = x

score :: Rocks -> Int
score = S.foldr (\p -> (offset - shiftR p 7 +)) 0
  where
    offset = 100

cycleRocks :: Fixed -> Rocks -> Rocks
cycleRocks fixed rocks = foldr (move fixed) rocks [east, south, west, north]

part1 :: Bool -> ByteString -> String
part1 _ input = show . score . move fixed north $ rocks
  where
    (rocks, fixed) =
      second makeVector . extractS . runParser parseInput () 0 $ input

part2 :: Bool -> ByteString -> String
part2 _ input = show pos
  where
    (rocks, fixed) =
      second makeVector . extractS . runParser parseInput () 0 $ input
    firstCycles = fmap score . iterateN 250 (cycleRocks fixed) $ rocks
    pat = findPattern 100 1 (==) firstCycles
    remainder = mod (numCycles - 100) pat
    pos = fromJust $ firstCycles !? (100 + remainder)
