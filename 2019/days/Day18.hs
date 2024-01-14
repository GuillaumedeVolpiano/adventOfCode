module Day18
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as A (UArray, array, bounds, indices, (!))
import           Data.Bifunctor     (second)
import           Data.Char          (isLetter, isLower, isUpper, toUpper)
import           Data.List          as L (filter, groupBy, map, sortBy, tails,
                                          unfoldr)
import           Data.Map           as M (Map, empty, fromList, size, (!))
import           Data.Sequence      as Sq (singleton)
import           Data.Set           as St (Set, empty, filter, fromList, insert,
                                           intersection, isSubsetOf, map,
                                           member, singleton, size, union)
import           Helpers.Graph      (Pos, dirs, neighbours)
import           Helpers.Parsers    (arrayFromString)
import           Helpers.Search     (bfs, dijkstraUncertainGoalDist)
import           Linear.V2          (V2 (..))

type Keys = Set Char

precalcKeys :: UArray Pos Char -> (Pos, Map Pos [(Pos, Set Char, Int)])
precalcKeys vault =
  ( startPoint
  , M.fromList .
    L.map (foldr (\(a, b) (_, c) -> (a, b : c)) (0, [])) .
    groupBy (\(a, _) (b, _) -> a == b) .
    sortBy (\(a, _) (b, _) -> compare a b) .
    concat .
    zipWith
      (\k l ->
         concatMap
           ((\(a, b, c, d) -> [(a, (b, c, d)), (b, (a, c, d))]) .
            (\p ->
               ( k
               , p
               , St.fromList . L.filter isLetter . L.map (vault A.!) . bfsed k $
                 p
               , (-1 +) . length . bfsed k $ p)))
           l)
      (startPoint : init keys) $
    tails keys)
  where
    allKeys =
      St.fromList . L.filter isLower . L.map (vault A.!) . indices $ vault
    startPoint = head . L.filter ((== '@') . (vault A.!)) . indices $ vault
    keys = L.filter (\p -> vault A.! p `member` allKeys) . indices $ vault
    bfsed from to =
      bfs (Sq.singleton from) (St.singleton from) M.empty neighb (== to)
    neighb = L.filter ((/= '#') . (A.!) vault) . neighbours vault

findAllKeys :: String -> Int
findAllKeys = findAllKeysInVault . arrayFromString

findAllKeysInVault :: UArray Pos Char -> Int
findAllKeysInVault vault =
  dijkstraUncertainGoalDist startKey 0 neighbours isGoal
  where
    (startPoint, preCalc) = precalcKeys vault
    startKey = (startPoint, St.singleton '@')
    isGoal = ((==) . M.size $ preCalc) . St.size . snd
    actualDoors =
      St.map toUpper .
      St.fromList . L.filter isLower . L.map (vault A.!) . indices $
      vault
    neighbours (node, keyring) =
      L.map
        (\(a, doors, dist) ->
           ( (a, union keyring . St.map toUpper . St.filter isLower $ doors)
           , dist)) .
      L.filter (`accessible` keyring) . (M.!) preCalc $
      node
    accessible (_, doors, _) keyring =
      (St.filter isUpper doors `intersection` actualDoors) `isSubsetOf` keyring &&
      not ((St.map toUpper . St.filter isLower $ doors) `isSubsetOf` keyring)

findAllKeysWithBots :: String -> Int
findAllKeysWithBots input = sum . L.map findAllKeysInVault $ fourArrays
  where
    originalVault = arrayFromString input
    (V2 osx osy) =
      head . L.filter ((== '@') . (A.!) originalVault) . indices $ originalVault
    (V2 mx my, V2 mX mY) = bounds originalVault
    makeSmallerArray :: (Int, Int) -> UArray Pos Char
    makeSmallerArray (a, b) =
      array (V2 (bmx a) (bmy b), V2 (bmX a) (bmY b)) $
      (V2 (osx + a) (osy + b), '@') :
      [ (V2 x y, '#')
      | x <- [osx + a, osx]
      , y <- [osy + b, osy]
      , x == osx || y == osy
      ] ++
      [ (V2 x y, originalVault A.! V2 x y)
      | x <- [bmx a .. bmX a]
      , y <- [bmy b .. bmY b]
      , not (x `elem` [osx, osx + a] && y `elem` [osy, osy + b])
      ]
    fourArrays = [makeSmallerArray (x, y) | x <- [-1, 1], y <- [-1, 1]]
    bmx (-1) = mx
    bmx 1    = osx
    bmy (-1) = my
    bmy 1    = osy
    bmX (-1) = osx
    bmX 1    = mX
    bmY (-1) = osy
    bmY 1    = mY

part1 :: Bool -> String -> String
part1 _ = show . findAllKeys

part2 :: Bool -> String -> String
part2 _ = show . findAllKeysWithBots
