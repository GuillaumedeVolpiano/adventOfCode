module Day18
  ( part1
  , part2
  , parseNumber
  , explode
  ) where

import           Control.Monad.State.Lazy (State, evalState, get, put, runState)
import           Data.Char                (isDigit)

import           Debug.Trace

data Number
  = Reg Int
  | Pair Number Number
  deriving (Eq)

data Zip
  = LeftZip Number
  | RightZip Number
  deriving (Show, Eq)

type NumberParser = State String Number

type Zipper = (Number, Int, [Zip])

instance Show Number where
  show (Reg x)    = show x
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

parseNumber :: NumberParser
parseNumber = do
  s <- get
  let (a:as) = s
      (number, remainder)
        | isDigit a = (Reg . read $ digits, restDigits)
        | a == '[' = runState parsePair as
        | a == ']' || a == ',' = runState parseNumber as
        where
          (digits, restDigits) = span isDigit s
  put remainder
  return number

goLeft :: Zipper -> Zipper
goLeft (Pair a b, depth, zipper) = (a, depth + 1, LeftZip b : zipper)

goRight :: Zipper -> Zipper
goRight (Pair a b, depth, zipper) = (b, depth + 1, RightZip a : zipper)

goUp :: Zipper -> Zipper
goUp (a, depth, (LeftZip b):zipper)  = (Pair a b, depth - 1, zipper)
goUp (b, depth, (RightZip a):zipper) = (Pair a b, depth - 1, zipper)

goTop :: Zipper -> Number
goTop (a, 0, []) = a
goTop zipper     = goTop . goUp $ zipper

isRight :: Zip -> Bool
isRight (RightZip _) = True
isRight _            = False

isLeft :: Zip -> Bool
isLeft (LeftZip _) = True
isLeft _           = False

addNumber :: Number -> Number -> Number
addNumber a = reduce . Pair a

reduce :: Number -> Number
reduce n
  | isDeep 4 n = trace ("explode " ++ show n) reduce . explode $ [(n, 0, [])]
  | has10 n = trace ("reduce " ++ show n) reduce . split $ [(n, 0, [])]
  | otherwise = n

addLeft :: Int -> [Zipper -> Zipper] -> Zipper -> Zipper
addLeft x l zip@(n, d, z)
  | all isLeft z = zip
addLeft x l zip@(n, d, z:zs)
  | isRight z = leftAdd x (goUp : goRight : l) . goLeft . goUp $ zip
  | otherwise = addUpThenRight x (goRight : l) . goUp $ zip

leftAdd :: Int -> [Zipper -> Zipper] -> Zipper -> Zipper
leftAdd x l zip@(Reg y, _, _) = goBack l . zipAdd x $ zip
leftAdd x l zip               = leftAdd x (goUp : l) . goLeft $ zip

rightAdd :: Int -> [Zipper -> Zipper] -> Zipper -> Zipper
rightAdd x l zip@(Reg y, _, _) = goBack l . zipAdd x $ zip
rightAdd x l zip               = rightAdd x (goUp : l) . goRight $ zip

addUpThenRight :: Int -> [Zipper -> Zipper] -> Zipper -> Zipper
addUpThenRight x l zip@(Reg y, _, _) = goBack l . zipAdd x $ zip
addUpThenRight x l zip@(_, _, z:zs)
  | isLeft z = addUpThenRight x (goLeft : l) . goUp $ zip
  | otherwise = rightAdd x (goUp : l) . goLeft $ zip

addUpThenLeft :: Int -> Zipper -> Number
addUpThenLeft x zip@(Reg y, _, _) = goTop . zipAdd x $ zip
addUpThenLeft x zip@(_, _, z:zs)
  | isRight z =
    trace ("still going up " ++ show zip) addUpThenLeft x . goUp $ zip
  | otherwise = goTop . leftAdd x [] . goRight . goUp $ zip

addRight :: Int -> Zipper -> Number
addRight x zip@(n, d, z)
  | all isRight z = goTop zip
addRight x zip@(n, d, z:zs)
  | isLeft z = goTop . rightAdd x [] . goRight . goUp $ zip
  | otherwise = addUpThenLeft x . goUp $ zip

zipAdd :: Int -> Zipper -> Zipper
zipAdd x (Reg y, d, z) = (Reg (x + y), d, z)

goBack :: [Zipper -> Zipper] -> Zipper -> Zipper
goBack l z = foldl (\a b -> b a) z l

explode :: [Zipper] -> Number
explode s
  | null s = error "no nested pair found"
explode ((Reg _, _, _):zs) = explode zs
explode ((Pair (Reg a) (Reg b), 4, zipper):_) =
  addRight b . addLeft a [] $ (Reg 0, 4, zipper)
explode (z:zs) = explode $ goLeft z : goRight z : zs

split :: [Zipper] -> Number
split s
  | null s = error "no number greater than 10 found"
split (z@(Pair _ _, _, _):zs) = split $ goLeft z : goRight z : zs
split ((Reg x, d, zip):zs)
  | x < 10 = split zs
  | otherwise = goTop (Pair (Reg (div x 2)) (Reg (div x 2 + mod x 2)), d, zip)

isDeep :: Int -> Number -> Bool
isDeep n (Reg _)    = False
isDeep 0 _          = True
isDeep n (Pair a b) = isDeep (n - 1) a || isDeep (n - 1) b

has10 :: Number -> Bool
has10 (Reg x)    = x >= 10
has10 (Pair a b) = has10 a || has10 b

parsePair :: NumberParser
parsePair = do
  firstNumber <- parseNumber
  Pair firstNumber <$> parseNumber

part1 :: Bool -> String -> String
part1 _ = show . foldl1 addNumber . map (evalState parseNumber) . lines

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
