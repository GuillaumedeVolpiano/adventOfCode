module Day12
  ( part1
  , part2
  ) where

import           Helpers.Parsers (custom, integers)

import           Data.Maybe      (Maybe (Just, Nothing), catMaybes)

testLine :: (String, [Int]) -> [Maybe String]
testLine (l, [])
  | '#' `elem` l = [Nothing]
  | otherwise = [Just (replicate (length l) '.')]
testLine ([], _) = [Nothing]
testLine (l@(a:as), record@(b:bs))
  -- if we have less ? and # than we need in total, we're going nowhere
  | length (filter (/= '.') l) < sum record = [Nothing]
  -- just absorb the dots and move on
  | a == '.' = insertRes dots (postDots, record)
  -- If the first character is a '#', we need to have a group of springs.
  -- If there are not enough potentials in the currently considered sublist to
  -- make the next group, we're going nowhere.
  | a == '#' && length notDots < b = [Nothing]
  -- If we have just enough potentials in the sublist to make the next group,
  -- then that's our only choice.
  | a == '#' && length notDots == b = insertRes (replicate b '#') (rest, bs)
  -- If there are more potentials than needed, then we need to refine further.
  | a == '#' = testForNext
  -- If the first character is neither a '.' nor a '#', it has to be a '?'
  -- If we don't have enough potentials to make the next group, but we know we
  -- have a spring, then we're on the wrong path.
  | length notDots < b && elem '#' notDots = [Nothing]
  -- if we do not have enough potentials to make a group and we don't have a
  -- certain spring, then all the '?' are actually '.'
  | length notDots < b = insertRes (replicate b '.') (rest, record)
  -- if we have exactly the right number of potentials and one of them is a
  -- spring, we have to have a group.
  | length notDots == b && elem '#' notDots =
    insertRes (replicate b '#') (rest, bs)
  -- if we have exactly the right number of potentials and all of them are '?',
  -- then either we have a group or we don't, and we move to the next sublist.
  | length notDots == b =
    insertRes (replicate b '#') (rest, bs) ++
    insertRes (replicate b '.') (rest, record)
  -- otherwise, the '?' can be either a '#' or a '.'. In the first case, we
  -- refine further, in the second, we just move forward by one character.
  | otherwise = testForNext ++ insertRes "." (as, record)
  where
    (dots, postDots) = span (== '.') l
    (notDots, rest) = span (/= '.') l
    --sel is a chain of b characters that may be a group. We know the first
    --character is a '#' because it is invoked by testForNext
    (sel, postSel) = splitAt b notDots
    --get the instruction after the current one, if it exists. Called by
    --testForNext
    (bn:_) = bs
    --We have a potentially valid chain, so we map concatenate it over
    --what happens next.
    insertRes res = map ((++) <$> Just res <*>) . testLine
    -- We know the first character is a '#' and that we have more than enough
    -- elements in the substring to create a group.
    testForNext
    -- if there are no instructions after that and there are no '#' left, we're
    -- done.
      | null bs && notElem '#' postSel && notElem '#' rest =
        [Just (replicate b '#' ++ replicate (length postSel + length rest) '.')]
    -- if there are no instructions after that and there are '#' left, then we
    -- have followed a wrong branch. Abort.
      | null bs = [Nothing]
    -- There are instructions left. Let's see if we need to consider the rest of
    -- the current sublist.
    -- If the length of the sublist is less than the sum of the current
    -- instruction and the next and we have leftover '#', then we are on a wrong
    -- branch, abort.
      | length l <= b + bn && elem '#' postSel = [Nothing]
    -- If the length of the sublist is less than the sum of the current
    -- instruction and the next and we do not have leftover '#', then we can
    -- just replace the rest of the sublist with '.'
      | length l <= b + bn =
        insertRes (replicate b '#' ++ replicate (length l - b) '.') (rest, bs)
    -- So the length of the sublist is greater than the sum of the current
    -- instruction and the next, which means that we can potentially squeeze a
    -- '.' and examine the rest of the substring. But we can't have a '#' right
    -- after our group, so we first check that.
      | head postSel == '#' = [Nothing]
    -- If we don't have a '#' right after our group, then we insert b '#' and a
    -- '.' and continue after removing (b+1) elements from the list.
      | otherwise = insertRes (replicate b '#' ++ ".") (drop (b + 1) l, bs)

part1 :: Bool -> String -> String
part1 _ input = show . sum . map (length . catMaybes . testLine) $ pairs
  where
    springs = map concat . custom "[?.#]+" $ input
    records = integers input
    pairs = zip springs records

part2 :: Bool -> String -> String
part2 _ input = show . sum . map (length . catMaybes . testLine) $ pairs
  where
    springs = map concat . custom "[?.#]+" $ input
    records = integers input
    unfoldedSprings =
      map (\t -> (t ++) . take (4 * (length t + 1)) . cycle $ ('?' : t)) springs
    unfoldedRecords = map (\t -> take (5 * length t) . cycle $ t) records
    pairs = zip unfoldedSprings unfoldedRecords
