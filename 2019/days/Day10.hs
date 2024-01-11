module Day10
  ( part1
  , part2
  ) where

import           Data.List (group, groupBy, maximumBy, nub, sort, sortBy, tails)
import           Data.List as L (filter, map)
import           Data.Set  as St (Set, delete, difference, empty, filter,
                                  foldl', fromList, insert, map, size, toList)
import           Linear.V2 (V2 (..))

type Pos = V2 Int

type AstMap = [Pos]

type Line = (Maybe Slope, Offset)

type Slope = Rational

type Offset = Rational

numBet = 200

aligned :: Line -> (Pos -> Bool) -> Pos -> Bool
aligned (Nothing, offset) half p@(V2 x _) = half p && fromIntegral x == offset
aligned (Just slope, offset) half p@(V2 x y) =
  half p && fromIntegral y == fromIntegral x * slope + offset

line :: Pos -> Pos -> Line
line from@(V2 xf yf) to@(V2 xt yt)
  | xf == xt = (Nothing, fromIntegral xf)
  | otherwise = (Just slope, offset)
  where
    slope = fromIntegral (yf - yt) / fromIntegral (xf - xt) :: Rational
    offset = fromIntegral yf - slope * fromIntegral xf

between :: Pos -> Pos -> Pos -> Bool
between from@(V2 xf yf) to@(V2 xt yt) pos@(V2 a b) =
  pos /= from && pos /= to && xm <= a && a <= xM && ym <= b && b <= yM
  where
    xm = min xf xt
    xM = max xf xt
    ym = min yf yt
    yM = max yf yt

selectVisible :: [(Pos, Pos)] -> [Pos]
selectVisible aList =
  unpair . L.filter (\(a, b) -> not . any (between a b) $ nubbed) $ aList
  where
    nubbed = fromList . unpair $ aList
    unpair = concatMap (\(a, b) -> [a, b])

allLines :: AstMap -> [(Pos, Pos, Line)]
allLines astMap =
  concat .
  zipWith
    (\a b -> L.map (\x -> (a, x, line a x)) . L.filter (/= a) $ b)
    (init astMap) $
  tails astMap

grouped :: [(Pos, Pos, Line)] -> [[Pos]]
grouped =
  group .
  sort .
  concatMap (selectVisible . L.map (\(a, b, _) -> (a, b))) .
  groupBy (\(_, _, a) (_, _, b) -> a == b) .
  sortBy (\(_, _, a) (_, _, b) -> compare a b)

findBest :: [(Pos, Pos, Line)] -> Int
findBest = maximum . L.map length . grouped

findZapped :: AstMap -> Int
findZapped astMap = score . zapPos (0, empty) . delete best . fromList $ astMap
  where
    score (V2 x y) = 100 * x + y
    linedUp = allLines astMap
    best =
      head . maximumBy (\a b -> compare (length a) (length b)) . grouped $
      linedUp
    bestLines =
      fromList .
      L.map (\(_, _, c) -> c) . L.filter (\(a, b, _) -> a == best || b == best) $
      linedUp
    zapPos preZapped surviving
      | length (snd preZapped) == numBet = fst preZapped
      | otherwise = zapNeg zapped (difference surviving (snd zapped))
      where
        zapped =
          foldl' (zap best (firstHalf best) surviving) preZapped bestLines
    zapNeg :: (Pos, Set Pos) -> Set Pos -> Pos
    zapNeg preZapped surviving
      | length preZapped == numBet = fst preZapped
      | otherwise = zapPos zapped (difference surviving (snd zapped))
      where
        zapped =
          foldl' (zap best (secondHalf best) surviving) preZapped bestLines

firstHalf :: Pos -> Pos -> Bool
firstHalf (V2 a b) (V2 c d) = (a == c && d < b) || c > a

secondHalf :: Pos -> Pos -> Bool
secondHalf (V2 a b) (V2 c d) = (a == c && d > b) || c < a

zap ::
     Pos -> (Pos -> Bool) -> Set Pos -> (Pos, Set Pos) -> Line -> (Pos, Set Pos)
zap best half surviving (last, zapped) lineOfFire
  | size zapped == numBet = (last, zapped)
  | otherwise = (new, insert new zapped)
  where
    inLine = St.filter (aligned lineOfFire half) surviving
    new
      | null inLine = last
      | otherwise =
        head . toList . St.filter (\x -> not . any (between best x) $ inLine) $
        inLine

buildMap :: String -> AstMap
buildMap =
  concat .
  zipWith (\a -> L.map (`V2` a)) [0 ..] .
  L.map (L.map fst . L.filter ((== '#') . snd) . zip [0 ..]) . lines

part1 :: Bool -> String -> String
part1 _ = show . findBest . allLines . buildMap

part2 :: Bool -> String -> String
part2 _ = show . findZapped . buildMap
