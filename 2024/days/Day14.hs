module Day14
  ( part1
  , part2
  ) where

import           Data.Bifunctor                   (bimap, first)
import           Data.List                        as L (filter, intercalate,
                                                        map, maximumBy,
                                                        partition, sortBy,
                                                        unfoldr)
import           Data.List.Split                  (chunksOf)
import           Data.Ord                         (comparing)
import           Data.Sequence                    as Sq (fromList)
import           Data.Set                         as S (Set, filter, fromList,
                                                        map, member, size)
import           Data.Text                        (Text)
import           Data.Tuple                       (swap)
import           Helpers.Parsers.Text             (signedInts)
import           Helpers.Search                   (findPattern)
import           Math.NumberTheory.Moduli.Chinese

data Robot =
  Robot X Y Dx Dy
  deriving (Show, Ord, Eq)

type X = Int

type Y = Int

type Dx = Int

type Dy = Int

width test
  | test = 11
  | otherwise = 101

height test
  | test = 7
  | otherwise = 103

area :: Set Robot -> Int
area = size . S.map pos

buildBot :: [Int] -> Robot
buildBot [x, y, dx, dy] = Robot x y dx dy

pos :: Robot -> (X, Y)
pos (Robot x y _ _) = (x, y)

render :: Set Robot -> String
render bots =
  unlines . chunksOf (width False)
    $ [ if (x, y) `member` S.map pos bots
        then '#'
        else '.'
      | y <- [0 .. height False - 1]
      , x <- [0 .. width False - 1]
      ]

second :: Robot -> Robot
second (Robot x y dx dy) =
  Robot ((x + dx) `mod` width False) ((y + dy) `mod` height False) dx dy

returnToPos :: Robot -> Int
returnToPos (Robot x y dx dy) = undefined
  where
    nx = div (gcd dx (width True)) dx
    ny = div (gcd dy (height True)) dy

part1 :: Bool -> Text -> String
part1 test =
  show
    . uncurry (*)
    . bimap
        (uncurry (*)
           . bimap length length
           . partition ((< div (height test) 2) . snd))
        (uncurry (*)
           . bimap length length
           . partition ((< div (height test) 2) . snd))
    . partition ((< div (width test) 2) . fst)
    . L.filter (\(x, y) -> x /= div (width test) 2 && y /= div (height test) 2)
    . L.map
        (\[x, y, dx, dy] ->
           ((x + 100 * dx) `mod` width test, (y + 100 * dy) `mod` height test))
    . signedInts

part2 :: Bool -> Text -> String
part2 _ =
  uncurry (++)
    . first render
    . swap
    . maximumBy (comparing (area . snd))
    . zip (L.map show [0 ..])
    . take tries
    . iterate (S.map second)
    . S.fromList
    . L.map buildBot
    . signedInts
  where
    (Just tries) =
      snd <$> chinese (0, width False) (0, height False) :: Maybe Int
