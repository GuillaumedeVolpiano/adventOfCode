module Day12
  ( part1
  , part2
  ) where

import           Helpers.Parsers (custom, integers)

import           Data.Foldable   (toList)
import           Data.List       as L (length)
import           Data.Map        as M (Map, adjust, empty, insert, lookup,
                                       member, singleton, (!))
import           Data.Maybe      (Maybe (Just, Nothing), catMaybes, fromJust,
                                  isJust, isNothing)
import           Data.Sequence   as Sq (Seq ((:<|), (:|>)), drop, elemIndexL,
                                        elemIndexR, empty, filter, fromList,
                                        length, replicate, singleton, spanl,
                                        splitAt, (><))

type Record = (Seq Char, [Int])

testLine ::
     Record
  -> Map Record (Seq (Maybe (Seq Char)))
  -> Map Record (Seq (Maybe (Seq Char)))
testLine c@(l, []) seen
  | M.member c seen = seen
  | '#' `elem` l = insert c (Sq.singleton Nothing) seen
  | otherwise =
    insert c (Sq.singleton . Just . Sq.replicate (Sq.length l) $ '.') seen
testLine c@(l, _) seen
  | null l = insert c (Sq.singleton Nothing) seen
testLine c@(l, record@(b:bs)) seen
  -- We've been there already
  | member c seen = seen
  -- if we have less ? and # than we need in total, we're going nowhere
  | Sq.length (Sq.filter (/= '.') l) < sum record =
    insert c (Sq.singleton Nothing) seen
  -- just absorb the dots and move on
  | a == '.' = insertRes dots (postDots, record) seen
  -- If the first character is a '#', we need to have a group of springs.
  -- If there are not enough potentials in the currently considered sublist to
  -- make the next group, we're going nowhere.
  | a == '#' && Sq.length notDots < b = insert c (Sq.singleton Nothing) seen
  -- If we have just enough potentials in the sublist to make the next group,
  -- then that's our only choice.
  | a == '#' && Sq.length notDots == b =
    insertRes (Sq.replicate b '#') (rest, bs) seen
  -- If there are more potentials than needed, then we need to refine further.
  | a == '#' = testForNext
  -- If the first character is neither a '.' nor a '#', it has to be a '?'
  -- If we don't have enough potentials to make the next group, but we know we
  -- have a spring, then we're on the wrong path.
  | Sq.length notDots < b && elem '#' notDots =
    insert c (Sq.singleton Nothing) seen
  -- if we do not have enough potentials to make a group and we don't have a
  -- certain spring, then all the '?' are actually '.'
  | Sq.length notDots < b = insertRes (Sq.replicate b '.') (rest, record) seen
  -- if we have exactly the right number of potentials and one of them is a
  -- spring, we have to have a group.
  | Sq.length notDots == b && elem '#' notDots =
    insertRes (Sq.replicate b '#') (rest, bs) seen
  -- if we have exactly the right number of potentials and all of them are '?',
  -- then either we have a group or we don't, and we move to the next sublist.
  | Sq.length notDots == b =
    branch (Sq.replicate b '#') (rest, bs) $
    insertRes (Sq.replicate b '.') (rest, record) seen
  -- if we don't have enough potentials to squeeze in the current and the next
  -- set of instructions (or if there is only one set of instructions left)
  -- and there is at least one #, then the starting point
  -- cannot be further than (b-1) positions from the last #. So if there is a #
  -- before that, we are on a wrong branch. Else, we can just skip until we're
  -- in a possible position.
  | (null bs || b + bn + 1 > Sq.length l) && isJust rir && (ir - il > b) =
    insert c (Sq.singleton Nothing) seen
  | (null bs || b + bn + 1 > Sq.length l) && isJust rir && ir > b =
    insertRes (Sq.replicate (ir - b) '.') (Sq.drop (ir - b) l, record) seen
  -- otherwise, the '?' can be either a '#' or a '.'. In the first case, we
  -- refine further, in the second, we just move forward by one character.
  | otherwise = branch (Sq.singleton '.') (as, record) testForNext
  where
    (a :<| as) = l
    (dots, postDots) = spanl (== '.') l
    (notDots, rest) = spanl (/= '.') l
    rir = elemIndexR '#' l
    Just ir = rir
    Just il = elemIndexL '#' l
    --sel is a chain of b characters that may be a group. We know the first
    --character is a '#' because it is invoked by testForNext
    (sel, postSel) = Sq.splitAt b notDots
    (p :<| _) = postSel
    --get the instruction after the current one, if it exists. Called by
    --testForNext
    (bn:_) = bs
    --We have a potentially valid chain, so we map concatenate it over
    --what happens next.
    insertRes res nextPos dic
    -- Been there before
      | member nextPos dic =
        insert c (fmap ((><) <$> Just res <*>) (dic ! nextPos)) dic
    -- Down the rabbit hole
      | otherwise =
        insert c (fmap ((><) <$> Just res <*>) . (!) newDic $ nextPos) newDic
      -- Here we go, here we go
      where
        newDic = testLine nextPos seen
    branch res pos dic =
      insert c (fmap ((><) <$> Just res <*>) newBranch >< newDic ! c) newDic
      where
        newDic
          | member pos dic = dic
          | otherwise = testLine pos dic
        newBranch = newDic ! pos
    -- We know the first character is a '#' and that we have more than enough
    -- elements in the substring to create a group.
    testForNext
    -- if there are no instructions after that and there are no '#' left, we're
    -- done.
      | null bs && notElem '#' postSel && notElem '#' rest =
        insert
          c
          (Sq.singleton $
           Just
             (Sq.replicate b '#' ><
              Sq.replicate (Sq.length postSel + Sq.length rest) '.'))
          seen
    -- if there are no instructions after that and there are '#' left, then we
    -- have followed a wrong branch. Abort.
      | null bs = insert c (Sq.singleton Nothing) seen
    -- There are instructions left. Let's see if we need to consider the rest of
    -- the current sublist.
    -- If the length of the sublist is less than the sum of the current
    -- instruction and the next and we have leftover '#', then we are on a wrong
    -- branch, abort.
      | Sq.length l <= b + bn && elem '#' postSel =
        insert c (Sq.singleton Nothing) seen
    -- If the length of the sublist is less than the sum of the current
    -- instruction and the next and we do not have leftover '#', then we can
    -- just replace the rest of the sublist with '.'
      | Sq.length l <= b + bn =
        insertRes
          (Sq.replicate b '#' >< Sq.replicate (Sq.length l - b) '.')
          (rest, bs)
          seen
    -- So the length of the sublist is greater than the sum of the current
    -- instruction and the next, which means that we can potentially squeeze a
    -- '.' and examine the rest of the substring. But we can't have a '#' right
    -- after our group, so we first check that.
      | p == '#' = insert c (Sq.singleton Nothing) seen
    -- If we don't have a '#' right after our group, then we insert b '#' and a
    -- '.' and continue after removing (b+1) elements from the list.
      | otherwise =
        insertRes (Sq.replicate b '#' :|> '.') (Sq.drop (b + 1) l, bs) seen

part1 :: Bool -> String -> String
part1 _ input =
  show . sum . map (L.length . catMaybes . toList . (\x -> testLine x M.empty ! x)) $
  pairs
  where
    springs = map (fromList . concat) . custom "[?.#]+" $ input
    records = integers input
    pairs = zip springs records

part2 :: Bool -> String -> String
part2 _ input = "Part 2"
--   show .
--   sum . map (L.length . catMaybes . toList . (\x -> testLine x M.empty ! x)) $
--   pairs
--   where
--     springs = map concat . custom "[?.#]+" $ input
--     records = integers input
--     unfoldedSprings =
--       map
--         (\t ->
--            fromList . (t ++) . take (4 * (L.length t + 1)) . cycle $ ('?' : t))
--         springs
--     unfoldedRecords = map (\t -> take (5 * L.length t) . cycle $ t) records
--     pairs = zip unfoldedSprings unfoldedRecords
