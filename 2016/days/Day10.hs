{-# LANGUAGE TupleSections #-}

module Day10
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (first, second)
import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Data.IntMap          as M (IntMap, alter, delete, empty,
                                            filter, insert, keys, member, null,
                                            (!))
import           Data.Maybe           (isJust)
import           Data.Text            (Text)
import           Helpers.Parsers.Text (Parser, decimal, string)
import           Text.Megaparsec      (eof, optional, parse, takeWhile1P, (<|>))
import           Text.Megaparsec.Char (char, eol)

data Robot =
  Robot Chip Chip
  deriving (Show, Eq, Ord)

newtype Output =
  Output Chip
  deriving (Show, Eq, Ord)

type Chip = Maybe Int

type Outputs = IntMap Output

type Robots = IntMap Robot

type IsRobot = Bool

type Instructions = IntMap (Lower, Higher)

type Lower = (IsRobot, Int)

type Higher = (IsRobot, Int)

type State = ((Robots, Outputs), Instructions)

class Receiver a where
  receive :: Int -> Maybe a -> Maybe a

instance Receiver Robot where
  receive chip Nothing = Just . Robot (Just chip) $ Nothing
  receive chip (Just (Robot Nothing Nothing)) =
    Just . Robot (Just chip) $ Nothing
  receive chip2 (Just (Robot (Just chip1) Nothing)) =
    Just . Robot (Just . min chip1 $ chip2) $ (Just . max chip1 $ chip2)
  receive _ _ = error "Robot full"

instance Receiver Output where
  receive chip Nothing                 = Just . Output . Just $ chip
  receive chip (Just (Output Nothing)) = Just . Output . Just $ chip
  receive _ _                          = error "Output full"

alterReceive ::
     (Receiver a, Receiver b)
  => (Bool, Int)
  -> Int
  -> (IntMap a, IntMap b)
  -> (IntMap a, IntMap b)
alterReceive (True, k) v ms = first (alter (receive v) k) ms
alterReceive (_, k) v ms    = second (alter (receive v) k) ms

give :: Int -> State -> State
give index (receivers, instructions) =
  (first (delete index) receivers', instructions)
  where
    (Robot (Just lower) (Just higher)) = fst receivers ! index
    (lowDest, highDest) = instructions ! index
    receivers' =
      alterReceive lowDest lower . alterReceive highDest higher $ receivers

isFull :: Robot -> Bool
isFull (Robot a b) = isJust a && isJust b

parseInput :: Parser (Robots, Instructions)
parseInput =
  parseValue
    <|> parseInst
    <|> (do
           eof
           return (empty, empty))

parseValue :: Parser (Robots, Instructions)
parseValue = do
  char 'v'
  consume
  chip <- decimal
  consume
  bot <- decimal
  optional eol
  first (alter (receive chip) bot) <$> parseInput

parseInst :: Parser (Robots, Instructions)
parseInst = do
  string "bot "
  bot <- decimal
  string " gives low to "
  low <- parseOutput <|> parseBot
  string " and high to "
  high <- parseOutput <|> parseBot
  optional eol
  second (insert bot (low, high)) <$> parseInput

parseOutput :: Parser (Bool, Int)
parseOutput = do
  string "output "
  (False, ) <$> decimal

parseBot :: Parser (Bool, Int)
parseBot = do
  string "bot "
  (True, ) <$> decimal

consume :: Parser ()
consume = do
  takeWhile1P Nothing (not . isDigit)
  return ()

process1 :: State -> Int
process1 state@((robots, _), instructions)
  | not . M.null $ targetBot = head . keys $ targetBot
  | otherwise = process1 . foldr give state . keys $ fullBots
  where
    targetBot = M.filter (== Robot (Just 17) (Just 61)) robots
    fullBots = M.filter isFull robots

process2 :: State -> Int
process2 state@((robots, outputs), instructions)
  | M.null fullBots =
    product . map ((\(Output (Just x)) -> x) . (outputs !)) $ [0 .. 2]
  | otherwise = process2 . foldr give state . keys $ fullBots
  where
    fullBots = M.filter isFull robots

part1 :: Bool -> Text -> String
part1 _ =
  show
    . process1
    . first (, empty)
    . fromRight (empty, empty)
    . parse parseInput ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . process2
    . first (, empty)
    . fromRight (empty, empty)
    . parse parseInput ""
