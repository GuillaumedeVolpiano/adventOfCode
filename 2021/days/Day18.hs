module Day18
  ( part1
  , part2
  ) where

import           Control.Monad.State.Lazy (State, evalState, get, put, runState)
import           Data.Char                (isDigit)
import           Data.Maybe               (Maybe (Just, Nothing), catMaybes)

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
  | isDeep 4 n = reduce . explode $ [(n, 0, [])]
  | has10 n = reduce . split $ [(n, 0, [])]
  | otherwise = n

leftAdd :: Int -> Zipper -> Zipper
leftAdd x zip@(Reg y, _, _) = zipAdd x zip
leftAdd x zip               = leftAdd x . goLeft $ zip

rightAdd :: Int -> Zipper -> Zipper
rightAdd x zip@(Reg y, _, _) = zipAdd x zip
rightAdd x zip               = rightAdd x . goRight $ zip

addUpThenRight :: Int -> Zipper -> Number
addUpThenRight x zip@(_, _, z:zs)
  | isLeft z && all isLeft zs = goTop zip
  | isLeft z = addUpThenRight x . goUp $ zip
  | otherwise = goTop . rightAdd x . goLeft . goUp $ zip

addUpThenLeft :: Int -> Zipper -> Number
addUpThenLeft x zip@(_, _, z:zs)
  | isRight z && all isRight zs = goTop zip
  | isRight z = addUpThenLeft x . goUp $ zip
  | otherwise = goTop . leftAdd x . goRight . goUp $ zip

addLeft :: Int -> Zipper -> Zipper
addLeft x (Pair a b, d, z) = (Pair addedLeft (Reg 0), d, z)
  where
    addedLeft = goTop . rightAdd x $ (a, 0, [])

addRight :: Int -> Zipper -> Zipper
addRight x (Pair a b, d, z) = (Pair (Reg 0) addedRight, d, z)
  where
    addedRight = goTop . leftAdd x $ (b, 0, [])

zipAdd :: Int -> Zipper -> Zipper
zipAdd x (Reg y, d, z) = (Reg (x + y), d, z)

goBack :: [Zipper -> Zipper] -> Zipper -> Zipper
goBack l z = foldl (\a b -> b a) z l

explode :: [Zipper] -> Number
explode s
  | null s = error "no nested pair found"
explode ((Reg _, _, _):zs) = explode zs
explode (zip@(Pair (Reg a) (Reg b), 4, z:zs):_)
  | isRight z = addUpThenLeft b . addLeft a . goUp $ zip
  | isLeft z = addUpThenRight a . addRight b . goUp $ zip
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

magnitude :: Number -> Int
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b
magnitude (Reg a)    = a

checkMagnitude :: Number -> Number -> Maybe Int
checkMagnitude a b
  | a == b = Nothing
  | otherwise = Just . magnitude . addNumber a $ b

allMagnitudes :: [Number] -> [Maybe Int]
allMagnitudes nums = checkMagnitude <$> nums <*> nums

part1 :: Bool -> String -> String
part1 _ =
  show . magnitude . foldl1 addNumber . map (evalState parseNumber) . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  maximum . catMaybes . allMagnitudes . map (evalState parseNumber) . lines
