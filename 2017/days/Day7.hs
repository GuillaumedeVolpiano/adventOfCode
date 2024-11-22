module Day7
  ( part1
  , part2
  ) where

import           Data.List       ((\\))
import           Helpers.Parsers (alphaNum)

data Tower = Program
  { name        :: String
  , weight      :: Int
  , totalWeight :: Int
  , subTowers   :: [Tower]
  } deriving (Show, Eq)

findRoot :: [[String]] -> String
findRoot l = head $ names \\ contents
  where
    names = map head l
    contents = concatMap (drop 2) l

buildTower :: [[String]] -> Tower
buildTower l = tower l . findRoot $ l

tower :: [[String]] -> String -> Tower
tower l n = Program n w tw st
  where
    st = map (tower l) subs
    tw = (+) w . sum . map totalWeight $ st
    (_:rawW:subs) = head . filter ((== n) . head) $ l
    w = read rawW

balance :: Int -> Tower -> Int
balance curDiff tower
  | null sub || minV == maxV = curDiff + weight tower
  | otherwise = balance newDiff nextTower
  where
    sub = subTowers tower
    minV = minimum . map totalWeight $ sub
    maxV = maximum . map totalWeight $ sub
    minima = filter ((== minV) . totalWeight) sub
    newDiff
      | length minima == 1 = maxV - minV
      | otherwise = minV - maxV
    nextTower
      | length minima == 1 = head minima
      | otherwise = head . filter ((== maxV) . totalWeight) $ sub

part1 :: Bool -> String -> String
part1 _ = show . findRoot . alphaNum

part2 :: Bool -> String -> String
part2 _ = show . balance 0 . buildTower . alphaNum
