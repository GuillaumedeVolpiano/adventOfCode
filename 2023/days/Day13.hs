{-# LANGUAGE TemplateHaskell #-}

module Day13
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, array, bounds, (!))
import           Data.ByteString            (ByteString)
import Data.List (uncons)
import           Data.Maybe                 (Maybe (Just, Nothing), fromJust,
                                             isJust)
import           FlatParse.Stateful         (ask, char, eof, get, local,
                                             optional_, put, runParser, satisfy,
                                             some)
import qualified Helpers.Parsers.ByteString as B (lines)
import           Helpers.Parsers.FlatParse  (ParserS, extractS)
import           Linear.V2                  (V2 (..))

type Pos = V2 Int

type Pat = UArray Pos Char

data Direction
  = V
  | H
  deriving (Show, Eq, Ord)

parseInput :: ParserS Int [Pat]
parseInput = some parsePat <* eof

parsePat :: ParserS Int Pat
parsePat =
  put 0
    >> (concat <$> some parseLine)
    >>= \l ->
          optional_ $(char '\n')
            >> pure (array (V2 0 0, maximum . map fst $ l) l)

parseLine :: ParserS Int [(Pos, Char)]
parseLine =
  get >>= \y ->
    put 0
      >> (local (const y) . some $ parseChar)
      >>= \l -> put (y + 1) >> $(char '\n') >> pure l

parseChar :: ParserS Int (Pos, Char)
parseChar =
  ask >>= \y ->
    get >>= \x ->
      satisfy (`elem` ".#") >>= \c -> put (x + 1) >> pure (V2 x y, c)

findAxis :: Pat -> (Int, Direction)
findAxis pat
  | isJust va = (fromJust va, V)
  | isJust ha = (fromJust ha, H)
  | otherwise = error ("axis not found for pattern\n" ++ show pat)
  where
    va = findVerticalAxis 0 pat
    ha = findHorizontalAxis 0 pat

-- Our arrays start at 0 where in the problem they start at 1, so we need to add
-- 1 to the result we find
findVerticalAxis :: Int -> Pat -> Maybe Int
findVerticalAxis axis pat
  | axis >= width = Nothing
  | symmetric = Just (axis + 1)
  | otherwise = findVerticalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (width - axis)
    backward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [(axis + 1) .. (axis + term)]
    symmetric = and . zipWith (==) backward $ forward

findHorizontalAxis :: Int -> Pat -> Maybe Int
findHorizontalAxis axis pat
  | axis >= height = Nothing
  | symmetric = Just (axis + 1)
  | otherwise = findHorizontalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (height - axis)
    backward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [(axis + 1) .. (axis + term)]
    symmetric = and . zipWith (==) backward $ forward

findSmudgedAxis :: Pat -> (Int, Direction)
findSmudgedAxis pat
  | isJust va = (fromJust va, V)
  | isJust ha = (fromJust ha, H)
  where
    va = findSmudgedVerticalAxis 0 pat
    ha = findSmudgedHorizontalAxis 0 pat

findSmudgedVerticalAxis :: Int -> Pat -> Maybe Int
findSmudgedVerticalAxis axis pat
  | axis >= width = Nothing
  | quasiSymmetric = Just (axis + 1)
  | otherwise = findSmudgedVerticalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (width - axis)
    backward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [(axis + 1) .. (axis + term)]
    diffPairs = filter (uncurry (/=)) . zip backward $ forward
    justTheOne = length diffPairs == 1
    Just ((bef, aft), _) = uncons diffPairs
    smudged = (length . filter (uncurry (/=)) . zip bef $ aft) == 1
    quasiSymmetric = justTheOne && smudged

findSmudgedHorizontalAxis :: Int -> Pat -> Maybe Int
findSmudgedHorizontalAxis axis pat
  | axis >= height = Nothing
  | quasiSymmetric = Just (axis + 1)
  | otherwise = findSmudgedHorizontalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (height - axis)
    backward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [(axis + 1) .. (axis + term)]
    diffPairs = filter (uncurry (/=)) . zip backward $ forward
    justTheOne = length diffPairs == 1
    Just ((bef, aft), _) = uncons diffPairs
    smudged = (length . filter (uncurry (/=)) . zip bef $ aft) == 1
    quasiSymmetric = justTheOne && smudged

score :: (Int, Direction) -> Int
score (i, V) = i
score (i, _) = i * 100

part1 :: Bool -> ByteString -> String
part1 _ =
  show . sum . map (score . findAxis) . extractS . runParser parseInput 0 0

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . sum
    . map (score . findSmudgedAxis)
    . extractS
    . runParser parseInput 0 0
