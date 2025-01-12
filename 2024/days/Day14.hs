module Day14
  ( part1
  , part2
  ) where

import           Data.Bifunctor                   (bimap, first)
import           Data.ByteString                  (ByteString)
import           Data.List                        (minimumBy, partition)
import           Data.List.Split                  (chunksOf)
import           Data.Maybe                       (fromJust)
import           Data.Ord                         (comparing)
import           Data.Tuple                       (swap)
import           Data.Vector                      (Vector, generate)
import           Helpers.Parsers.ByteString       (signedInts)
import           Math.NumberTheory.Moduli.Chinese (chinese)
import           Statistics.Sample                (variance)

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

buildBot :: [Int] -> Robot
buildBot [x, y, dx, dy] = Robot x y dx dy

pos :: Robot -> (X, Y)
pos (Robot x y _ _) = (x, y)

botsAtSec :: Int -> Bool -> [Robot] -> [Robot]
botsAtSec sec test =
  map
    (\(Robot x y dx dy) ->
       Robot
         ((x + sec * dx) `mod` width test)
         ((y + sec * dy) `mod` height test)
         dx
         dy)

chineseFindTree :: [Robot] -> String
chineseFindTree robots =
  (render . botsAtSec treeSec False $ robots) ++ show treeSec
  where
    treeSec =
      fst . fromJust . chinese (xSecond, width False) $ (ySecond, height False)
    botSeconds = take 103 . map (map pos) . iterate (map second) $ robots
    xSecond =
      fst
        . minimumBy (comparing (variance . snd))
        . zip [0 ..]
        . map
            ((\list -> generate (width False) (list !!))
               . map (fromIntegral . fst))
        $ botSeconds
    ySecond =
      fst
        . minimumBy (comparing (variance . snd))
        . zip [0 ..]
        . map
            ((\list -> generate (height False) (list !!))
               . map (fromIntegral . snd))
        $ botSeconds

render :: [Robot] -> String
render bots =
  unlines . chunksOf (width False)
    $ [ if (x, y) `elem` map pos bots
        then '#'
        else '.'
      | y <- [0 .. height False - 1]
      , x <- [0 .. width False - 1]
      ]

second :: Robot -> Robot
second (Robot x y dx dy) =
  Robot ((x + dx) `mod` width False) ((y + dy) `mod` height False) dx dy

part1 :: Bool -> ByteString -> String
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
    . filter (\(x, y) -> x /= div (width test) 2 && y /= div (height test) 2)
    . map pos
    . botsAtSec 100 test
    . map buildBot
    . signedInts

part2 :: Bool -> ByteString -> String
part2 _ = chineseFindTree . map buildBot . signedInts
