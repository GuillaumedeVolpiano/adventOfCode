{-# LANGUAGE TupleSections #-}

module Day19
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Map        as M (Map, fromList, (!))
import           Data.Maybe      (Maybe (Just, Nothing), catMaybes, isNothing)
import           Text.Regex.TDFA (getAllTextMatches, (=~))

import           Debug.Trace

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
    { xRange :: Range
    , mRange :: Range
    , aRange :: Range
    , sRange :: Range
    }
  deriving (Show)

type Range = Maybe (Int, Int)

type Workflow = String

type Condition = (Part -> Bool)

type RangeCondition = (PartRange -> (PartRange, PartRange))

type System = Map Workflow [(Condition, Workflow)]

type SystemRange = Map Workflow [(RangeCondition, Workflow)]

type Accepted = [Part]

type AcceptedRange = [PartRange]

toCat = M.fromList [("x", x), ("m", m), ("a", a), ("s", s)]

toComp = M.fromList [("<", (<)), (">", (>))]

range = Just (1, 4000)

initialRange = PartRange range range range range

emptyRange = PartRange Nothing Nothing Nothing Nothing

empty :: PartRange -> Bool
empty (PartRange a b c d) =
  isNothing a || isNothing b || isNothing c || isNothing d

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
    (a, b, c) = s =~ "[<>]"

rangeThreeParse :: String -> RangeCondition
rangeThreeParse s = newPartRange
  where
    (a, b, c) = s =~ "[<>]"
    val = read c
    newPartRange
      | a == "x" = xSplit b val
      | a == "m" = mSplit b val
      | a == "a" = aSplit b val
      | a == "s" = sSplit b val

xSplit :: String -> Int -> PartRange -> (PartRange, PartRange)
xSplit comp val (PartRange xr mr ar sr) =
  (PartRange yr mr ar sr, PartRange nr mr ar sr)
  where
    (yr, nr) = split comp val xr

mSplit :: String -> Int -> PartRange -> (PartRange, PartRange)
mSplit comp val (PartRange xr mr ar sr) =
  (PartRange xr yr ar sr, PartRange xr nr ar sr)
  where
    (yr, nr) = split comp val mr

aSplit :: String -> Int -> PartRange -> (PartRange, PartRange)
aSplit comp val (PartRange xr mr ar sr) =
  (PartRange xr mr yr sr, PartRange xr mr nr sr)
  where
    (yr, nr) = split comp val ar

sSplit :: String -> Int -> PartRange -> (PartRange, PartRange)
sSplit comp val (PartRange xr mr ar sr) =
  (PartRange xr mr ar yr, PartRange xr mr ar nr)
  where
    (yr, nr) = split comp val sr

split ::
     String -> Int -> Maybe (Int, Int) -> (Maybe (Int, Int), Maybe (Int, Int))
split comp val (Just (minv, maxv))
  | comp == "<" && minv < val && maxv < val = (Just (minv, maxv), Nothing)
  | comp == "<" && minv < val = (Just (minv, val - 1), Just (val, maxv))
  | comp == "<" = (Nothing, Just (minv, maxv))
  | minv > val && maxv > val = (Just (minv, maxv), Nothing)
  | maxv > val = (Just (val + 1, maxv), Just (minv, val))
  | otherwise = (Nothing, Just (minv, maxv))

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
partRangeSize (PartRange xr mr ar sr) = size xr * size mr * size ar * size sr

size :: Maybe (Int, Int) -> Int
size Nothing       = 0
size (Just (a, b)) = b - a + 1

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
