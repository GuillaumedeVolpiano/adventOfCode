{-# LANGUAGE TupleSections #-}

module Day20
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Array.Unboxed   as A (UArray, array, bounds, inRange,
                                            indices, (!))
import           Data.Bifunctor       (second)
import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Data.List            as L (delete, filter, group, groupBy,
                                            intercalate, intersect, nub, sort,
                                            sortBy, unfoldr, (\\))
import           Data.Map             as M (Map, elems, filter, fromList, keys,
                                            (!))
import           Helpers.Parsers      (Parser)
import           Linear.V2            (V2 (..))
import           Text.Megaparsec      (count, eof, many, manyTill, optional,
                                       parse, sepBy, skipManyTill, skipSome,
                                       takeWhile1P)
import           Text.Megaparsec.Char (char, digitChar, eol, printChar)

type Id = Int

type Tile = UArray Pos Bool

type Pos = V2 Int

type IdToTile = Map Id Tile

type SideToId = Map Side [Id]

type Side = (Int, Int)

type Image = UArray Pos Bool

range = [0 .. 9]

monster =
  [ V2 0 1
  , V2 1 2
  , V2 4 2
  , V2 5 1
  , V2 6 1
  , V2 7 2
  , V2 10 2
  , V2 11 1
  , V2 12 1
  , V2 13 2
  , V2 16 2
  , V2 17 1
  , V2 18 0
  , V2 18 1
  , V2 19 1
  ]

boolToInt :: [Bool] -> Int
boolToInt =
  foldl
    (\a b ->
       if b
         then 1 + 2 * a
         else 2 * a)
    0

boolToSide :: [Bool] -> Side
boolToSide side = (min bts rbts, max bts rbts)
  where
    bts = boolToInt side
    rbts = boolToInt . reverse $ side

rotate :: Tile -> Tile
rotate tile =
  array b [(V2 x y, tile A.! V2 (my - y) x) | x <- [0 .. mx], y <- [0 .. my]]
  where
    b@(_, V2 mx my) = bounds tile

rotations :: Tile -> [Tile]
rotations = take 4 . iterate rotate

flipTile :: Tile -> Tile
flipTile tile =
  array b [(V2 x y, tile A.! V2 (mx - x) y) | x <- [0 .. mx], y <- [0 .. my]]
  where
    b@(_, V2 mx my) = bounds tile

flippedRotations :: Tile -> [Tile]
flippedRotations = map flipTile . rotations

findRotation :: Tile -> Side -> Side -> Tile
findRotation tile botSide rightSide = rotated
  where
    pot = rotations tile ++ flippedRotations tile
    rotated =
      head . L.filter (\t -> botVal t == botSide && rightVal t == rightSide) $
      pot

rightVal :: Tile -> Side
rightVal t = boolToSide [t A.! V2 9 y | y <- range]

topVal :: Tile -> Side
topVal t = boolToSide [t A.! V2 x 0 | x <- range]

leftVal :: Tile -> Side
leftVal t = boolToSide [t A.! V2 0 y | y <- range]

botVal :: Tile -> Side
botVal t = boolToSide [t A.! V2 x 9 | x <- range]

parser :: Parser [(Id, Tile)]
parser = manyTill parseTileId eof

parseTileId :: Parser (Id, Tile)
parseTileId = do
  id <- parseId
  tile <- parseTile
  optional eol
  return (id, tile)

parseId :: Parser Id
parseId = do
  id <- read <$> skipManyTill printChar (takeWhile1P Nothing isDigit)
  char ':'
  eol
  return id

parseTile :: Parser Tile
parseTile = makeTile <$> count 10 (many printChar <* eol)

makeTile :: [String] -> Tile
makeTile lines =
  array
    (V2 0 0, V2 9 9)
    [(V2 x y, (lines !! y !! x) == '#') | x <- range, y <- range]

tileToSides :: Tile -> [Side]
tileToSides tile = map (\x -> x tile) [leftVal, rightVal, topVal, botVal]

buildSideToId :: [(Id, Tile)] -> SideToId
buildSideToId =
  fromList .
  map (second nub . foldl (\(_, b) (c, d) -> (c, d : b)) ((0, 0), [])) .
  groupBy (\a b -> fst a == fst b) .
  sortBy (\a b -> compare (fst a) (fst b)) .
  concatMap ((\(a, b) -> map (, a) b) . second tileToSides)

findLaterals :: SideToId -> [Id]
findLaterals = concat . elems . M.filter ((== 1) . length)

findAngles :: [Id] -> [Id]
findAngles = map head . L.filter ((== 2) . length) . group . sort

assemble :: [(Id, Tile)] -> Image
assemble tiles =
  cropImage . reverse . map reverse . unfoldr (unfoldImage sideToId idToTile) $
  (map fst tiles, [])
  where
    sideToId = buildSideToId tiles
    idToTile = fromList tiles

cropImage :: [[Tile]] -> Image
cropImage tiles =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, valAt x y) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = (-1 +) . (cropWidth *) . length . head $ tiles
    height = cropWidth * length tiles - 1
    valAt x y =
      widthArray (heightArray y) x A.!
      V2 (1 + mod x cropWidth) (1 + mod y cropWidth)
    heightArray y = tiles !! div y cropWidth
    widthArray line x = line !! div x cropWidth
    cropWidth = 8

unfoldImage ::
     SideToId -> IdToTile -> ([Id], [Tile]) -> Maybe ([Tile], ([Id], [Tile]))
unfoldImage sideToId idToTile (remTiles, prevLine)
  | null remTiles = Nothing
  | otherwise = Just (map snd newLine, (newRemTiles, map snd newLine))
  where
    newLine =
      unfoldr (unfoldLine sideToId idToTile) (remTiles, (-1, -1), prevLine)
    newRemTiles = remTiles \\ map fst newLine

unfoldLine ::
     SideToId
  -> IdToTile
  -> ([Id], Side, [Tile])
  -> Maybe ((Id, Tile), ([Id], Side, [Tile]))
unfoldLine sideToId idToTile (remTiles, rawSide, prevLine)
  | rawSide /= (-1, -1) && length (sideToId M.! side) == 1 = Nothing
  | otherwise = Just ((newTileId, newTile), (newRem, leftVal newTile, newPrev))
  where
    newPrev
      | null prevLine = prevLine
      | otherwise = tail prevLine
    newTileId
      | rawSide == (-1, -1) && null prevLine =
        head . findAngles . findLaterals $ sideToId
      | rawSide == (-1, -1) =
        head . L.filter (`elem` remTiles) . (sideToId M.!) . topVal . head $
        prevLine
      | otherwise = head . L.filter (`elem` remTiles) $ sideToId M.! rawSide
    newRem = delete newTileId remTiles
    rawNewTile = idToTile M.! newTileId
    side
      | rawSide == (-1, -1) && null prevLine =
        head . L.filter ((== 1) . length . (M.!) sideToId) . tileToSides $
        rawNewTile
      | rawSide == (-1, -1) =
        head .
        L.filter
          (\x ->
             (oppSide x /= botNewTile) && ((== 1) . length . (M.!) sideToId $ x)) .
        tileToSides $
        rawNewTile
      | otherwise = rawSide
    botNewTile
      | null prevLine =
        head .
        L.filter (`notElem` [side, oppSide side]) .
        keys . M.filter (\x -> length x == 1 && newTileId `elem` x) $
        sideToId
      | otherwise = topVal . head $ prevLine
    newTile = findRotation rawNewTile botNewTile side
    oppSide aSide
      | rightVal rawNewTile == aSide = leftVal rawNewTile
      | leftVal rawNewTile == aSide = rightVal rawNewTile
      | topVal rawNewTile == aSide = botVal rawNewTile
      | otherwise = topVal rawNewTile

roughness :: Image -> Int
roughness image = allHash - length monster * length monsters
  where
    images = rotations image ++ flippedRotations image
    potMonsters = map (\x -> map (x +) monster) . indices $ image
    monsterIsThere :: [Pos] -> Image -> Bool
    monsterIsThere monster ri = all (\y -> inRange b y && ri A.! y) monster
    monsters = L.filter (\x -> any (monsterIsThere x) images) potMonsters
    b = bounds image
    allHash = length . L.filter (image A.!) . indices $ image

part1 :: Bool -> String -> String
part1 _ =
  show .
  product .
  findAngles . findLaterals . buildSideToId . fromRight [] . parse parser ""

part2 :: Bool -> String -> String
part2 _ = show . roughness . assemble . fromRight [] . parse parser ""
