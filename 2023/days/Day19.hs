{-# LANGUAGE TupleSections #-}

module Day19
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Map        (Map, fromList, (!))
import           Data.Maybe      (Maybe (Just, Nothing), catMaybes)
import           Text.Regex.TDFA (getAllTextMatches, (=~))

data Part =
  Part
    { x :: Int
    , m :: Int
    , a :: Int
    , s :: Int
    }
  deriving (Show)

data PartRange =
  PartRange
    { xRange :: [Int]
    , mRange :: [Int]
    , aRange :: [Int]
    , sRange :: [Int]
    }
  deriving (Show)

type Workflow = String

type Condition = (Part -> Bool)

type RangeCondition = (PartRange -> (PartRange, PartRange))

type System = Map Workflow [(Condition, Workflow)]

type SystemRange = Map Workflow [(RangeCondition, Workflow)]

type Accepted = [Part]

type AcceptedRange = [PartRange]

toCat = fromList [("x", x), ("m", m), ("a", a), ("s", s)]

toComp = fromList [("<", (<)), (">", (>)), ("=", (==))]

range = [1 .. 4000]

initialRange = PartRange range range range range

emptyRange = PartRange [] [] [] []

empty :: PartRange -> Bool
empty (PartRange a b c d) = null a || null b || null c || null d

toDicTuple :: [String] -> (Workflow, [(Condition, Workflow)])
toDicTuple (s:ss) = (s, conditionParse ss)

toRangeDicTuple :: [String] -> (Workflow, [(RangeCondition, Workflow)])
toRangeDicTuple (s:ss) = (s, rangeConditionParse ss)

conditionParse :: [String] -> [(Condition, Workflow)]
conditionParse (a:as)
  | null as = [(const True, a)]
conditionParse (a:b:as) = (threeParse a, b) : conditionParse as

rangeConditionParse :: [String] -> [(RangeCondition, Workflow)]
rangeConditionParse (a:as)
  | null as = [((, emptyRange), a)]
rangeConditionParse (a:b:as) = (rangeThreeParse a, b) : rangeConditionParse as

threeParse :: String -> (Part -> Bool)
threeParse s = \part -> (toComp ! b) ((toCat ! a) part) (read c)
  where
    (a, b, c) = s =~ "[<>=]"

rangeThreeParse :: String -> RangeCondition
rangeThreeParse s = newPartRange
  where
    (a, b, c) = s =~ "[<>=]"
    val = read c
    newPartRange
      | a == "x" =
        \(PartRange xr mr ar sr) ->
          ( PartRange (filter (\v -> (toComp ! b) v val) xr) mr ar sr
          , PartRange (filter (\v -> not ((toComp ! b) v val)) xr) mr ar sr)
      | a == "m" =
        \(PartRange xr mr ar sr) ->
          ( PartRange xr (filter (\v -> (toComp ! b) v val) mr) ar sr
          , PartRange xr (filter (\v -> not ((toComp ! b) v val)) mr) ar sr)
      | a == "a" =
        \(PartRange xr mr ar sr) ->
          ( PartRange xr mr (filter (\v -> (toComp ! b) v val) ar) sr
          , PartRange xr mr (filter (\v -> not ((toComp ! b) v val)) ar) sr)
      | a == "s" =
        \(PartRange xr mr ar sr) ->
          ( PartRange xr mr ar (filter (\v -> (toComp ! b) v val) sr)
          , PartRange xr mr ar (filter (\v -> not ((toComp ! b) v val)) sr))

makePart :: [String] -> Part
makePart s = Part a b c d
  where
    [a, b, c, d] = map (\x -> read (x =~ "[0-9]+")) s

toTuple :: [[[String]]] -> ([[String]], [[String]])
toTuple [a, b] = (a, b)

inputParser :: ([[String]], [[String]]) -> (System, [Part])
inputParser (a, b) = (fromList . map toDicTuple $ a, map makePart b)

rangeParser :: [[String]] -> SystemRange
rangeParser = fromList . map toRangeDicTuple

processAll :: Accepted -> (System, [Part]) -> Accepted
processAll proc (system, []) = proc
processAll proc (system, p:ps) =
  processAll (process proc system p "in") (system, ps)

process :: Accepted -> System -> Part -> Workflow -> Accepted
process proc system part wf
  | processed == "A" = part : proc
  | processed == "R" = proc
  | otherwise = process proc system part processed
  where
    processed = processOne part $ system ! wf

processOne :: Part -> [(Condition, Workflow)] -> Workflow
processOne p ((condition, workflow):xs)
  | condition p = workflow
  | otherwise = processOne p xs

score :: Part -> Int
score (Part x m a s) = x + m + a + s

processAllRange :: AcceptedRange -> SystemRange -> AcceptedRange
processAllRange ar system = processRange ar system initialRange "in"

processRange ::
     AcceptedRange -> SystemRange -> PartRange -> Workflow -> AcceptedRange
processRange accepted system partRange workflow = finalAccepted
  where
    processes = system ! workflow
    processProcesses (ar, pr, tp) p =
      let (na, processed, unprocessed) = processOneRange ar tp p
       in (na, processed : pr, unprocessed)
    (intAccepted, rawToProcess, _) =
      foldl processProcesses (accepted, [], Just partRange) processes
    stillToProcess = catMaybes rawToProcess
    finalAccepted =
      foldl
        (\a b -> uncurry (processRange a system) b)
        intAccepted
        stillToProcess

partRangeSize :: PartRange -> Int
partRangeSize (PartRange xr mr ar sr) =
  length xr * length mr * length ar * length sr

processOneRange ::
     AcceptedRange
  -> Maybe PartRange
  -> (RangeCondition, Workflow)
  -> (AcceptedRange, Maybe (PartRange, Workflow), Maybe PartRange)
processOneRange accepted Nothing _ = (accepted, Nothing, Nothing)
processOneRange accepted (Just partRange) (condition, workflow)
  | empty processed || workflow == "R" =
    (accepted, Nothing, testEmpty unprocessed)
  | workflow == "A" = (processed : accepted, Nothing, testEmpty unprocessed)
  | otherwise = (accepted, Just (processed, workflow), testEmpty unprocessed)
  where
    (processed, unprocessed) = condition partRange
    testEmpty pr
      | empty pr = Nothing
      | otherwise = Just pr

part1 :: Bool -> String -> String
part1 _ =
  show .
  sum .
  map score .
  processAll [] .
  inputParser .
  toTuple .
  splitWhen null .
  map (\s -> getAllTextMatches (s =~ "[a-zA-Z0-9<>=]+") :: [String]) . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  sum .
  map partRangeSize .
  processAllRange [] .
  rangeParser .
  head .
  splitWhen null .
  map (\s -> getAllTextMatches (s =~ "[a-zA-Z0-9<>=]+") :: [String]) . lines
