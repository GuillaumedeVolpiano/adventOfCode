module Day12
  ( part1
  , part2
  ) where

import           Data.Bifunctor  (first, second)
import           Data.List       (group, intercalate, isInfixOf, isPrefixOf,
                                  sort)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, empty, insert, member, (!))
import           Data.Maybe      (Maybe (Just, Nothing), isNothing, mapMaybe)
import           Data.Set        (Set, fromList, size)
import           Helpers.Parsers (custom, integers)

import           Debug.Trace

extractPatterns :: Map String [([Int], Int)] -> ([String], [Int]) -> Int
extractPatterns dic (ss, c)
  | null c && any ('#' `elem`) ss = 0
  | null ss && not (null c) = 0
  | null ss || null c = 1
  | s `member` dic = fromDic
  | isNothing rawResult = 0
  | otherwise = move grouped
  where
    (s:xs) = ss
    rawResult = extractPatternsMech (s, c)
    (Just result) = rawResult
    minGroups = length . filter ('#' `elem`) $ xs
    totalHash = length . filter (== '#') . concat $ xs
    grouped =
      filter (\(a, _) -> (sum a >= totalHash) && length a >= minGroups) .
      map (\a -> (head a, length a)) . group . sort $
      result
    newVals = map (\(a, b) -> (take (length c - length a) c, b)) grouped
    move = sum . map (\(a, b) -> b * extractPatterns newDic (xs, a))
    seen = filter (\(a, _) -> a `isPrefixOf` c) . (!) dic $ s
    fromDic
      | null seen && isNothing rawResult = 0
      | null seen = move grouped
      | otherwise = move $ map (\(a, b) -> (drop (length a) c, b)) seen
    newDic
      | not (member s dic) = insert s newVals dic
      | null seen = insert s (newVals ++ dic ! s) dic
      | otherwise = dic

extractPatternsMech :: (String, [Int]) -> Maybe [[Int]]
extractPatternsMech (s, c)
  | null c && '#' `elem` s = Nothing
  | null s || null c = Just [c]
  | length s < b && '#' `elem` s = Nothing
  | length s < b = Just [c]
  | a == '#' && length s == b = Just [bs]
  | length s == b && '#' `elem` s = Just [bs]
  | length s == b = Just [bs, c]
  | a == '#' && head postPat == '#' = Nothing
  | a == '#' = extractPatternsMech (tail postPat, bs)
  | head postPat == '#' = extractPatternsMech (as, c)
  | otherwise =
    combine
      (extractPatternsMech (tail postPat, bs))
      (extractPatternsMech (as, c))
  where
    combine Nothing t         = t
    combine t Nothing         = t
    combine (Just x) (Just y) = Just (x ++ y)
    (a:as) = s
    (b:bs) = c
    (curPat, postPat) = splitAt b s

prunePattern :: String -> [Int] -> String
prunePattern s l
  | replicate m '#' `isInfixOf` s = newS
  | otherwise = s
  where
    m = maximum l
    newS =
      intercalate (('.' : replicate m '#') ++ ".") .
      splitOn (('?' : replicate m '#') ++ "?") $
      s

part1 :: Bool -> String -> String
part1 _ input =
  trace (show . filter (\(a, _) -> length a == 1) $ pairs) show .
  sum . map (extractPatterns empty) $
  pairs
  where
    springs = custom "[?#]+" input
    records = integers input
    pairs = zip springs records

part2 :: Bool -> String -> String
part2 _ input =
  show .
  sum .
  map
    (extractPatterns empty .
     (\(a@(x:_), b) ->
        if length a == 1
          then (concat . custom "[?#]+" . prunePattern x $ b, b)
          else (a, b))) $
  pairs
  where
    springs = map concat . custom "[?#.]+" $ input
    records = integers input
    unfoldedSprings =
      concatMap
        (\t ->
           custom "[?#]+" . (t ++) . take (4 * (length t + 1)) . cycle $
           ('?' : t))
        springs
    unfoldedRecords = map (\t -> take (5 * length t) . cycle $ t) records
    pairs = zip unfoldedSprings unfoldedRecords
