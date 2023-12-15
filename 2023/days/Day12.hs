module Day12
  ( part1
  , part2
  ) where

import           Data.Bifunctor                         (first, second)
import           Data.List                              (group, inits,
                                                         intercalate,
                                                         intersperse, isInfixOf,
                                                         isPrefixOf, sort,
                                                         tails)
import           Data.List.Split                        (splitOn)
import           Data.Map                               (Map, empty, insert,
                                                         member, (!))
import           Data.Maybe                             (Maybe (Just, Nothing),
                                                         catMaybes, isNothing,
                                                         mapMaybe)
import           Data.Set                               (Set, fromList, size)
import           Helpers.Parsers                        (custom, integers)
import           Math.NumberTheory.Recurrences.Bilinear (binomialLine)

extractPatterns :: ([String], [Int]) -> Int
extractPatterns (s, [])
  -- we have consumed all the elements and have some '#' left. This was a wrong
  -- branch
  | any ('#' `elem`) s = 0
  -- we have consumed all instructions and all '#'
  | all ('#' `notElem`) s = 1
extractPatterns ([], l)
  -- we have consumed all the strings and have some instructions left. This was a
  -- wrong branch
  | not (null l) = 0
extractPatterns (s, l)
  -- we can't fit all the patterns in the remaining strings. Wrong branch
  | totalPatternSize (map length s) < totalPatternSize l = 0
extractPatterns (s@('#':as):ss, l@(b:ls))
  -- we can't fit the pattern in the current string. This was a wrong branch
  | length s < b = 0
  -- the string has exactly the pattern length. We have a pattern
  | length s == b = 1 * extractPatterns (ss, ls)
  -- the first term after the supposed pattern is a '#'. This is not allowed, so
  -- this was a wrong branch
  | s !! b == '#' = 0
  -- otherwise, we consume the pattern and move along. There is only one way to
  -- extract the pattern. (1 *) implied.
  | otherwise = extractPatterns (drop (b + 1) s : ss, ls)
-- The first term of the current string is a '?'
extractPatterns ([s], l@(b:ls))
  -- We have no hashes and exactly the right space to fit our remaining pattern
  | '#' `notElem` s && length s == totalPatternSize l = 1
  -- We have no hashes and more space than needed. Defer to the noHash algorithm.
  | '#' `notElem` s && length s > totalPatternSize l = noHash s l
extractPatterns (f@(s:ss), l@(b:ls))
  -- We can't fit the next pattern in the string and there are no hashes.
  -- Just consume it.
  | length s < b && '#' `notElem` s = extractPatterns (ss, l)
  -- We can't fit the next pattern in the string and there are hashes. This was a
  -- wrong branch.
  | length s < b = 0
  -- We don't have hashes, but have more than one string. We can try to fit
  -- patterns as long as they fit in the string.
  | '#' `notElem` s = sum . zipWith (*) possPotPatterns $ possRemPatterns
  -- We have at least a hash, but the size of the string is exactly that of the
  -- first pattern, so there is only one possibility, just consume the string and
  -- the pattern
  | length s == b = extractPatterns (ss, ls)
  -- We have at least a hash, and the string is longer than the first pattern
  | otherwise =
    sum .
    map
      (\((a, b), (c, d)) ->
         extractPatterns ([a], b) * extractPatterns (c : ss, d)) $
    toExplore
  where
    potPatterns = filter (\t -> totalPatternSize t <= length s) . inits $ l
    remPatterns = take (length potPatterns) . tails $ l
    possPotPatterns = map (noHash s) potPatterns
    possRemPatterns = map (\a -> extractPatterns (ss, a)) remPatterns
    (beforeHashes, fromHashes) = span (/= '#') s
    (firstHashes, afterHashes) = span (== '#') fromHashes
    -- The best scenario is one where the hash we see is actually the first
    -- hash, and thus the ? before is a point. So we need to see what patterns
    -- we can fit in the gap. We need < as we need to keep space for a .
    consumablePatterns =
      filter (/= l) .
      filter (\t -> totalPatternSize t < length beforeHashes) . inits $
      l
    unconsumedPatterns = take (length consumablePatterns) . tails $ l
    -- Then, the next pattern would be the one for our hash(es)
    nextPatterns = map head unconsumedPatterns
    remainingPatterns = map tail unconsumedPatterns
    triads = zip3 consumablePatterns nextPatterns remainingPatterns
    -- prune all the situations where the size of nextPattern is smaller than
    -- our number of hashes
    prunedTriads = filter (\(_, a, _) -> a >= length firstHashes) triads
    toExplore =
      concatMap
        (triadToPairs s (length firstHashes) beforeHashes afterHashes)
        prunedTriads

-- if we have x hashes and y in the pattern, then we need to consider all
-- cases from x being at the beginning of the pattern to x being at the end
-- of the pattern, so take [y - x  + 1, y - x .. 1] from beforeHashes (to
-- leave space for the dot) and [1
-- .. y - x + 1] from afterHashes, which we can refactor as map (y -x + 2)
-- [1 .. y -x + 1] and [1 .. y - x + 1]. We must also be careful to limit
-- ourselves to actually available slots before and after our hashes group.
-- If the string after our hashes is empty, then all our hashes need to be
-- consumed before.
-- And, finally, exclude any pattern where the next character would be a
-- hash or any situation where the initial ? can't absorb the pattern.
triadToPairs ::
     String
  -> Int
  -> String
  -> String
  -> ([Int], Int, [Int])
  -> [((String, [Int]), (String, [Int]))]
triadToPairs s numHashes beforeString afterString (before, hashLength, after) =
  catMaybes $ eatAll : eatBefore : eatAfter : baseCase
  where
    needHash = hashLength - numHashes + 1
    baseCase =
      [ Just ((crop k beforeString, before), (drop l afterString, after))
      | k <- [1 .. needHash]
      , l <- [1 .. needHash]
      , k + l == needHash + 1
      , k <= length beforeString
      , l <= length afterString
      , afterString !! (l - 1) /= '#'
      ]
    eatAfter
      | needHash > length afterString &&
          needHash - length afterString <= length beforeString =
        Just
          ( (crop (needHash - length afterString) beforeString, before)
          , ("", after))
      | otherwise = Nothing
    eatBefore
      | needHash > length beforeString &&
          needHash - length beforeString <= length afterString &&
          afterString !! (needHash - length beforeString - 1) /= '#' =
        Just
          ( ("", before)
          , (drop (needHash - length beforeString) afterString, after))
      | otherwise = Nothing
    -- here, we don't need to make room for a '.' at all
    eatAll
      | needHash == length beforeString + length afterString + 1 =
        Just (("", before), ("", after))
      | otherwise = Nothing

totalPatternSize :: [Int] -> Int
-- The size of the total remaining pattern is calculated by adding a dot to each
-- pattern but the last, and then summing the sizes, which is the same as
-- summing the sizes and adding length - 1.
totalPatternSize l = sum l + length l - 1

-- We have exactly one way to fit no pattern in a string. Otherwise, we have
-- choose (difference + number of slots - 1, number of slots - 1)
noHash :: String -> [Int] -> Int
noHash s [] = 1
noHash s l
  | diff < 0 =
    error
      ("the string can't accomodate the pattern\n" ++ show s ++ "\n" ++ show l)
  | otherwise = binomialLine (fromIntegral diff + numSlots) !! numSlots
    -- The length of our all '?' string
  where
    objLength = length s
    diff = objLength - totalPatternSize l
    -- We actually have one more slot, but the formula beeing
    -- choose (n + k -1, k -1), we don't need to add it.
    numSlots = length l

crop :: Show a => Int -> [a] -> [a]
crop n l
  | n <= 0 = l
  | n > length l =
    error
      ("can't take " ++
       show n ++ " out of list " ++ show l ++ " with length " ++ show (length l))
  | otherwise = take (length l - n) l

part1 :: Bool -> String -> String
part1 _ input = show . sum . map extractPatterns $ pairs
  where
    springs = custom "[?#]+" input
    records = integers input
    pairs = zip springs records

part2 :: Bool -> String -> String
part2 _ input = show . sum . map extractPatterns $ pairs
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
