{-# LANGUAGE TemplateHaskell #-}

module Day14
  ( part1
  , part2
  ) where

import           Control.Monad             (forM_, replicateM, when)
import           Control.Monad.ST          (ST, runST)
import           Data.Array.ST             (STUArray, freeze, getBounds,
                                            readArray, writeArray)
import           Data.Array.Unboxed        (UArray, array, bounds, (!))
import           Data.Array.Unsafe         (unsafeFreeze, unsafeThaw)
import           Data.ByteString           (ByteString)
import           Data.Ix                   (inRange)
import           Data.Maybe                (fromJust)
import           Data.Sequence             ((!?))
import qualified Data.Sequence             as Sq (fromList)
import           Data.STRef                (STRef, modifySTRef, newSTRef,
                                            readSTRef)
import           FlatParse.Stateful        (ask, char, eof, get, local, modify,
                                            put, runParser, satisfy, (<|>))
import           Helpers.Parsers.FlatParse (ParserS, extractS)
import           Helpers.Search            (findPattern)
import           Linear.V2                 (V2 (..))

import           Debug.Trace

type Position = V2 Int

north = V2 0 1

south = V2 0 (-1)

east = V2 1 0

west = V2 (-1) 0

numCycles = 1000000000

parseInput :: ParserS Int [(Position, Char)]
parseInput = (put 1 >> parseLine) <|> (eof >> pure [])

parseLine :: ParserS Int [(Position, Char)]
parseLine = parseChar <|> ($(char '\n') >> local (+ (-1)) parseInput)

parseChar :: ParserS Int [(Position, Char)]
parseChar =
  ask >>= \y ->
    get >>= \x ->
      put (x + 1)
        >> satisfy (`elem` ".#O")
        >>= \c -> ((V2 x y, c) :) <$> parseLine

move :: Position -> STUArray s Position Char -> ST s Int
move dir dish = do
  (_, V2 mx my) <- getBounds dish
  let order
        | dir == north = [V2 x y | x <- [1 .. mx], y <- [my,my - 1 .. 1]]
        | dir == west = [V2 x y | y <- [1 .. my], x <- [1 .. mx]]
        | dir == south = [V2 x y | x <- [1 .. mx], y <- [1 .. my]]
        | dir == east = [V2 x y | y <- [1 .. my], x <- [mx,mx - 1 .. 1]]
  score <- newSTRef 0
  forM_ order $ \pos ->
    readArray dish pos >>= \cur ->
      when (cur == 'O') $ moveOne dir dish score pos
  readSTRef score

moveOne ::
     Position -> STUArray s Position Char -> STRef s Int -> Position -> ST s ()
moveOne dir dish score pos@(V2 _ y) =
  getBounds dish >>= \bounds ->
    if inRange bounds (pos + dir)
      then testNext dir dish score pos
      else modifySTRef score (+ y)

testNext ::
     Position -> STUArray s Position Char -> STRef s Int -> Position -> ST s ()
testNext dir dish score pos@(V2 _ y) =
  readArray dish (pos + dir) >>= \next ->
    if next == '.'
      then writeArray dish pos '.'
             >> writeArray dish (pos + dir) 'O'
             >> moveOne dir dish score (pos + dir)
      else modifySTRef score (+ y)

moveNorth :: UArray Position Char -> ST s Int
moveNorth dish = unsafeThaw dish >>= \d -> move north d

cycleRocks :: STUArray s Position Char -> ST s Int
cycleRocks dish =
  move north dish >> move west dish >> move south dish >> move east dish

testCycles :: UArray Position Char -> String
testCycles dish =
  prettyPrint array1 ++ "\n" ++ prettyPrint array2 ++ "\n" ++ prettyPrint array3
  where
    (array1, array2, array3) =
      runST $ do
        d <- unsafeThaw dish
        cycleRocks d
        d1 <- freeze d
        cycleRocks d
        d2 <- freeze d
        cycleRocks d
        d3 <- freeze d
        pure (d1, d2, d3)

firstCycles' :: UArray Position Char -> ST s [Int]
firstCycles' dish = do
  d <- unsafeThaw dish
  replicateM 250 $ cycleRocks d

prettyPrint :: UArray Position Char -> String
prettyPrint dish =
  unlines [[dish ! V2 x y | x <- [1 .. mx]] | y <- [my,my - 1 .. 1]]
  where
    (_, V2 mx my) = bounds dish

part1 :: Bool -> ByteString -> String
part1 test input =
  show
    $ runST
    $ moveNorth
        . array (V2 1 1, V2 max max)
        . extractS
        . runParser parseInput max 1
    $ input
  where
    max
      | test = 10
      | otherwise = 100

part2 :: Bool -> ByteString -> String
part2 test input = show pos
  where
    dish =
      array (V2 1 1, V2 max max) . extractS . runParser parseInput max 1 $ input
    max
      | test = 10
      | otherwise = 100
    firstCycles = Sq.fromList $ runST $ firstCycles' dish
    pat = findPattern 100 1 (==) firstCycles
    remainder = mod (numCycles - 100) pat
    pos = fromJust $ firstCycles !? (99 + remainder)
