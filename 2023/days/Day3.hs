module Day3
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed    (UArray, array, bounds, inRange, indices,
                                        (!))
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (index)
import qualified Data.ByteString.Char8 as B (length, lines)
import           Data.List             (nub, sortBy, uncons)
import           Data.Maybe            (fromJust, isNothing, mapMaybe)
import           Linear.V2             (V2 (..))

type Pos = V2 Int

type Schematics = UArray Pos Char

type Part = Int

surrounds = [V2 x y | x <- [(-1) .. 1], y <- [(-1) .. 1], x /= 0 || y /= 0]

nextTo = [V2 (-1) 0, V2 1 0]

digits = "0123456789"

comparePos :: Pos -> Pos -> Ordering
comparePos (V2 v w) (V2 x y)
  | w == y = compare v x
  | otherwise = compare w y

digitNextToSymbol :: Schematics -> Pos -> Bool
digitNextToSymbol schematics pos =
  (schematics ! pos `elem` digits) && any (isSymbol . (pos +)) surrounds
  where
    schemBounds = bounds schematics
    isSymbol neighbour =
      inRange schemBounds neighbour
        && (schematics ! neighbour) `notElem` ('.' : digits)

isGear :: Schematics -> Pos -> Maybe [Int]
isGear schematics pos@(V2 a b)
  | schematics ! pos == '*' && length potParts == 2 = Just potParts
  | otherwise = Nothing
  where
    potParts =
      map (read . map (schematics !)) . nub . map (catPartDigits schematics)
        $ filter (\x -> (schematics ! x) `elem` digits) . map (pos +)
        $ surrounds

catPartDigits :: Schematics -> Pos -> [Pos]
catPartDigits schematics pos = catBefore (pos + V2 (-1) 0) $ catAfter pos
  where
    schemBounds = bounds schematics
    catPartDigits pos = catBefore (pos + V2 (-1) 0) $ catAfter pos
    catAfter pos
      | inRange schemBounds pos && schematics ! pos `elem` digits =
        pos : catAfter (pos + V2 1 0)
      | otherwise = []
    catBefore pos l
      | inRange schemBounds pos && schematics ! pos `elem` digits =
        catBefore (pos + V2 (-1) 0) (pos : l)
      | otherwise = l

schemToParts :: Schematics -> [Int]
schemToParts schematics =
  map (read . map (schematics !))
    . nub
    . map (catPartDigits schematics)
    . filter (digitNextToSymbol schematics)
    . indices
    $ schematics

schem :: ByteString -> Schematics
schem input =
  array
    (V2 0 0, V2 width height)
    [ (V2 x y, (preSchem !! y) `index` x)
    | x <- [0 .. width]
    , y <- [0 .. height]
    ]
  where
    preSchem = B.lines input
    Just (h, _) = uncons preSchem
    width = B.length h - 1
    height = length preSchem - 1

part1 :: Bool -> ByteString -> String
part1 _ = show . sum . schemToParts . schem

part2 :: Bool -> ByteString -> String
part2 _ input =
  show . sum . map product . mapMaybe (isGear schematics) . indices $ schematics
  where
    schematics = schem input
