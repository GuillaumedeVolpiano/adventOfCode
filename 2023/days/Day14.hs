module Day14
  ( part1
  , part2
  ) where

import           Control.Monad             (forM_, replicateM, when)
import           Control.Monad.ST          (ST, runST)
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (unpack)
import qualified Data.ByteString.Char8     as B (lines)
import           Data.Massiv.Array         (Array, Comp (Seq), Ix2 ((:.)), P,
                                            Sz (Sz, Sz2), fromLists', loopA_)
import           Data.Massiv.Array.Mutable (MArray, sizeOfMArray, write)
import qualified Data.Massiv.Array.Mutable as A (read)
import           Data.Massiv.Array.Unsafe  (unsafeFreeze, unsafeThaw)
import           Data.Maybe                (fromJust)
import           Data.Sequence             ((!?))
import qualified Data.Sequence             as Sq (fromList)
import           Data.STRef                (STRef, modifySTRef, newSTRef,
                                            readSTRef)
import           FlatParse.Stateful        (ask, char, eof, get, local, modify,
                                            put, runParser, satisfy, (<|>))
import           Helpers.Parsers.FlatParse (ParserS, extractS)
import           Helpers.Search            (findPattern)

type Position = Ix2

east = 0 :. 1

west = 0 :. (-1)

north = 1 :. 0

south = (-1) :. 0

numCycles = 1000000000

move :: Position -> MArray s P Position Char -> ST s Int
move dir dish = do
  let Sz (mx :. my) = sizeOfMArray dish
  score <- newSTRef 0
  let iterator1
        | dir == north = loopA_ (my - 1) (>= 0) (+ (-1))
        | dir == west = loopA_ 0 (< mx) (+ 1)
        | dir == south = loopA_ 0 (< my) (+ 1)
        | dir == east = loopA_ (mx - 1) (>= 0) (+ (-1))
      iterator2 coord f
        | dir == north || dir == south =
          loopA_ 0 (< mx) (+ 1) (\x -> f (coord :. x))
        | dir == east || dir == west =
          loopA_ 0 (< my) (+ 1) (\y -> f (y :. coord))
  iterator1
    (\coord ->
       iterator2
         coord
         (\pos ->
            A.read dish pos >>= \cur ->
              when (cur == Just 'O') $ moveOne dir dish score pos))
  readSTRef score

moveOne ::
     Position -> MArray s P Position Char -> STRef s Int -> Position -> ST s ()
moveOne dir dish score pos@(y :. _) =
  testNext dir dish pos >>= \test ->
    if test
      then write dish pos '.' >> moveOne dir dish score (pos + dir)
      else write dish pos 'O' >> modifySTRef score (+ (y + 1))

testNext :: Position -> MArray s P Position Char -> Position -> ST s Bool
testNext dir dish pos = (== Just '.') <$> A.read dish (pos + dir)

moveNorth :: Array P Position Char -> ST s Int
moveNorth dish = unsafeThaw dish >>= \d -> move north d

cycleRocks :: MArray s P Position Char -> ST s Int
cycleRocks dish =
  move north dish >> move west dish >> move south dish >> move east dish

firstCycles' :: Array P Position Char -> ST s [Int]
firstCycles' dish = do
  d <- unsafeThaw dish
  replicateM 250 $ cycleRocks d

part1 :: Bool -> ByteString -> String
part1 _ input =
  show
    $ runST
    $ moveNorth . fromLists' Seq . reverse . map unpack . B.lines
    $ input

part2 :: Bool -> ByteString -> String
part2 _ input = show pos
  where
    dish =
      fromLists' Seq . reverse . map unpack . B.lines $ input :: Array
        P
        Position
        Char
    firstCycles = Sq.fromList $ runST $ firstCycles' dish
    pat = findPattern 100 1 (==) firstCycles
    remainder = mod (numCycles - 100) pat
    pos = fromJust $ firstCycles !? (99 + remainder)
