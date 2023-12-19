{-# LANGUAGE TupleSections #-}

module Day19
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Map        as M (Map, fromList, (!))
import           Data.Maybe      (Maybe (Just, Nothing), catMaybes, fromJust,
                                  isNothing)
import           Text.Regex.TDFA (getAllTextMatches, (=~))

-- data types
data Part a =
  Part
    { x :: Maybe a
    , m :: Maybe a
    , a :: Maybe a
    , s :: Maybe a
    }

data Range =
  Range Int Int

-- types
type Workflow = String

type Condition a = (Part a -> (Part a, Part a))

type System a = Map Workflow [(Condition a, Workflow)]

type Accepted a = [Part a]

-- classes
class Parseable a where
  threeParse :: String -> Condition a
  size :: Part a -> Int

-- instances
instance Parseable Int where
  threeParse s = condition
    where
      (a, b, c) = s =~ "[<>]"
      condition part
        | (toComp ! b) (fromJust . (toCat ! a) $ part) (read c) =
          (part, emptyPart)
        | otherwise = (emptyPart, part)
  size p
    | empty p = 0
  size (Part (Just x) (Just m) (Just a) (Just s)) = x + m + a + s

instance Parseable Range where
  threeParse s = lSplit a b (read c)
    where
      (a, b, c) = s =~ "[<>]"
  size p
    | empty p = 0
  size (Part (Just x) (Just m) (Just a) (Just s)) =
    rangeSize x * rangeSize m * rangeSize a * rangeSize s

-- global variables
toCat = M.fromList [("x", x), ("m", m), ("a", a), ("s", s)]

toComp = M.fromList [("<", (<)), (">", (>))]

range = Just (Range 1 4000)

initialRange = Part range range range range

emptyPart = Part Nothing Nothing Nothing Nothing

-- helper functions for Parseable
lSplit :: String -> String -> Int -> Part Range -> (Part Range, Part Range)
lSplit cat comp val part
  | cat == "x" = (part {x = yr}, part {x = nr})
  | cat == "m" = (part {m = yr}, part {m = nr})
  | cat == "a" = (part {a = yr}, part {a = nr})
  | cat == "s" = (part {s = yr}, part {s = nr})
  where
    (yr, nr) = split comp val $ (toCat ! cat) part

split :: String -> Int -> Maybe Range -> (Maybe Range, Maybe Range)
split _ _ Nothing = (Nothing, Nothing)
split comp val (Just (Range minv maxv))
  | order minv val && order maxv val = (Just (Range minv maxv), Nothing)
  | order minv val = (Just (Range minv (val - 1)), Just (Range val maxv))
  | order maxv val = (Just (Range (val + 1) maxv), Just (Range minv val))
  | otherwise = (Nothing, Just (Range minv maxv))
  where
    order = toComp ! comp

rangeSize :: Range -> Int
rangeSize (Range a b) = b - a + 1

-- Main body
empty :: Part a -> Bool
empty (Part a b c d) = isNothing a || isNothing b || isNothing c || isNothing d

toDicTuple :: (Parseable a) => [String] -> (Workflow, [(Condition a, Workflow)])
toDicTuple (s:ss) = (s, conditionParse ss)

conditionParse :: (Parseable a) => [String] -> [(Condition a, Workflow)]
conditionParse (a:as)
  | null as = [((, emptyPart), a)]
conditionParse (a:b:as) = (threeParse a, b) : conditionParse as

makePart :: [String] -> Part Int
makePart s = Part (Just a) (Just b) (Just c) (Just d)
  where
    [a, b, c, d] = map (\x -> read (x =~ "[0-9]+")) s

toTuple :: [[[String]]] -> ([[String]], [[String]])
toTuple [a, b] = (a, b)

inputParser :: ([[String]], [[String]]) -> (System Int, [Part Int])
inputParser (a, b) = (fromList . map toDicTuple $ a, map makePart b)

rangeParser :: [[String]] -> (System Range, [Part Range])
rangeParser a = (fromList . map toDicTuple $ a, [initialRange])

processAll :: Accepted a -> (System a, [Part a]) -> Accepted a
processAll proc (system, []) = proc
processAll proc (system, p:ps) =
  processAll (process proc system p "in") (system, ps)

process :: Accepted a -> System a -> Part a -> Workflow -> Accepted a
process accepted system part workflow = finalAccepted
  where
    processes = system ! workflow
    processProcesses (ar, pr, tp) p =
      let (na, processed, unprocessed) = processOne ar tp p
       in (na, processed : pr, unprocessed)
    (intAccepted, rawToProcess, _) =
      foldl processProcesses (accepted, [], Just part) processes
    stillToProcess = catMaybes rawToProcess
    finalAccepted =
      foldl (\a b -> uncurry (process a system) b) intAccepted stillToProcess

processOne ::
     Accepted a
  -> Maybe (Part a)
  -> (Condition a, Workflow)
  -> (Accepted a, Maybe (Part a, Workflow), Maybe (Part a))
processOne accepted Nothing _ = (accepted, Nothing, Nothing)
processOne accepted (Just part) (condition, workflow)
  | empty processed || workflow == "R" =
    (accepted, Nothing, testEmpty unprocessed)
  | workflow == "A" = (processed : accepted, Nothing, testEmpty unprocessed)
  | otherwise = (accepted, Just (processed, workflow), testEmpty unprocessed)
  where
    (processed, unprocessed) = condition part
    testEmpty pr
      | empty pr = Nothing
      | otherwise = Just pr

part1 :: Bool -> String -> String
part1 _ =
  show .
  sum .
  map size .
  processAll [] .
  inputParser .
  toTuple .
  splitWhen null .
  map (\s -> getAllTextMatches (s =~ "[a-zA-Z0-9<>=]+") :: [String]) . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  sum .
  map size .
  processAll [] .
  rangeParser .
  head .
  splitWhen null .
  map (\s -> getAllTextMatches (s =~ "[a-zA-Z0-9<>=]+") :: [String]) . lines
