{-# LANGUAGE TupleSections #-}

-- data module for day 19
module Part
  ( Part
  , Range
  , Workflow
  , System
  , Accepted
  , Condition
  , empty
  , parseInput
  , size
  ) where

import           Data.List.Split (splitWhen)
import           Data.Map        (Map, fromList, (!))
import           Data.Maybe      (Maybe (Just, Nothing), fromJust, isNothing)
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
  parseInput :: String -> (System a, [Part a])

-- instances
instance Parseable Int where
  threeParse s = condition
    where
      (a, b, c) = s =~ "[<>]"
      condition part
        | compare (fromJust . (toCat ! a) $ part) (read c) == toComp ! b =
          (part, emptyPart)
        | otherwise = (emptyPart, part)
  size p
    | empty p = 0
  size (Part (Just x) (Just m) (Just a) (Just s)) = x + m + a + s
  parseInput =
    toTuple .
    splitWhen null .
    map (\s -> getAllTextMatches (s =~ "[a-zA-Z0-9<>=]+") :: [String]) . lines
    where
      toTuple [a, b] = (fromList . map toDicTuple $ a, map makePart b)
      makePart :: [String] -> Part Int
      makePart s = Part (Just a) (Just b) (Just c) (Just d)
        where
          [a, b, c, d] = map (\x -> read (x =~ "[0-9]+")) s

instance Parseable Range where
  threeParse s = lSplit
    where
      (a, b, c) = s =~ "[<>]"
      lSplit part
        | a == "x" = (part {x = yr}, part {x = nr})
        | a == "m" = (part {m = yr}, part {m = nr})
        | a == "a" = (part {a = yr}, part {a = nr})
        | a == "s" = (part {s = yr}, part {s = nr})
        where
          (yr, nr) = split $ (toCat ! a) part
      split Nothing = (Nothing, Nothing)
      split (Just (Range minv maxv))
        | compare minv val == order && compare maxv val == order =
          (Just (Range minv maxv), Nothing)
        | compare minv val == order =
          (Just (Range minv (val - 1)), Just (Range val maxv))
        | compare maxv val == order =
          (Just (Range (val + 1) maxv), Just (Range minv val))
        | otherwise = (Nothing, Just (Range minv maxv))
      order = toComp ! b
      val = read c
  size p
    | empty p = 0
  size (Part (Just x) (Just m) (Just a) (Just s)) =
    rangeSize x * rangeSize m * rangeSize a * rangeSize s
    where
      rangeSize (Range a b) = b - a + 1
  parseInput =
    toTuple .
    head .
    splitWhen null .
    map (\s -> getAllTextMatches (s =~ "[a-zA-Z0-9<>=]+") :: [String]) . lines
    where
      toTuple a = (fromList . map toDicTuple $ a, [initialRange])

-- global variables
toCat = fromList [("x", x), ("m", m), ("a", a), ("s", s)]

toComp = fromList [("<", LT), (">", GT)]

range = Just (Range 1 4000)

initialRange = Part range range range range

emptyPart = Part Nothing Nothing Nothing Nothing

-- helper functions
toDicTuple :: (Parseable a) => [String] -> (Workflow, [(Condition a, Workflow)])
toDicTuple (s:ss) = (s, conditionParse ss)

conditionParse :: (Parseable a) => [String] -> [(Condition a, Workflow)]
conditionParse (a:as)
  | null as = [((, emptyPart), a)]
conditionParse (a:b:as) = (threeParse a, b) : conditionParse as

empty :: Part a -> Bool
empty (Part a b c d) = isNothing a || isNothing b || isNothing c || isNothing d
