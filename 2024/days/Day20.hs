module Day20
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, array, (!?))
import           Data.Bits                  (shiftR, (.&.))
import           Data.ByteString            (ByteString, index)
import qualified Data.ByteString            as B (length)
import           Data.IntMap                (empty)
import qualified Data.IntSet                as S (singleton)
import           Data.List                  (foldl')
import           Data.Maybe                 (mapMaybe)
import           Data.Sequence              (singleton)
import           Data.Word                  (Word8)
import           Data.Word8                 (_E, _S, _period)
import qualified Helpers.Parsers.ByteString as P (lines)
import           Helpers.Search.Int         (bfsSafe)

type Pos = Int

type Maze = UArray Pos Word8

origin = 0

dirs = [1, -1, 256, -256]

threshold test
  | test = 50
  | otherwise = 100

manhattanDistance :: Int -> Int -> Int
manhattanDistance a b =
  abs ((a .&. 255) - (b .&. 255)) + abs (shiftR a 8 - shiftR b 8)

countCheats :: Bool -> Int -> ByteString -> Int
countCheats test cheatLast input =
  length . cheated test fromStart toEnd $ cheatLast
  where
    assocsMaze =
      [(x + 256 * y, l !! y `index` x) | x <- [0 .. width], y <- [0 .. height]]
    l = P.lines input
    height = length l - 1
    width = (-1 +) . B.length . head $ l
    maze = array (origin, width + 256 * height) assocsMaze
    start = fst . head . filter ((== _S) . snd) $ assocsMaze
    end = fst . head . filter ((== _E) . snd) $ assocsMaze
    (Just path) =
      reverse
        . fst
        . foldr (\p (ps, dist) -> ((p, dist) : ps, dist + 1)) ([], 0)
        <$> bfsSafe
              (singleton start)
              (S.singleton start)
              empty
              (neighbours maze)
              (== end) :: Maybe [(Int, Int)]
    fromStart = take (length path - threshold test - 1) path
    toEnd = drop (threshold test + 1) path

cheated :: Bool -> [(Int, Int)] -> [(Int, Int)] -> Int -> [(Int, Int)]
cheated test fromStart toEnd cheatLast =
  fst . foldl' (\(c, t) f -> (filter (isCheat f) t ++ c, tail t)) ([], toEnd)
    $ fromStart
  where
    calcSave (p, d) (p', d') = d' - d - manhattanDistance p p'
    isCheat p p' =
      manhattanDistance (fst p) (fst p') <= cheatLast
        && calcSave p p' >= threshold test

neighbours :: Maze -> Pos -> [Pos]
neighbours maze pos =
  filter (\p -> maze !? p `elem` [Just _period, Just _E]) . map (pos +) $ dirs

part1 :: Bool -> ByteString -> String
part1 test = show . countCheats test 2

part2 :: Bool -> ByteString -> String
part2 test = show . countCheats test 20
