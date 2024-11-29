module Day24
  ( part1
  , part2
  ) where

import           Data.Maybe      (isNothing, mapMaybe)
import           Data.Set        as S (Set, delete, empty, filter, findMax,
                                       fromList, insert, map, null, singleton,
                                       unions)
import           Helpers.Parsers (numbers)

data Port =
  Port Int Int
  deriving (Show)

type Pieces = Set Port

type Bridge = [Port]

instance Eq Port where
  (Port a b) == (Port c d) = (a == c && b == d) || (a == d && b == c)

instance Ord Port where
  compare (Port a b) (Port c d) =
    compare (min a b) (min c d) `mappend` compare (max a b) (max c d)

makePort :: [Int] -> Port
makePort [a, b] = Port a b

nextOpenPort :: Int -> Port -> Int
nextOpenPort a (Port b c)
  | a == b = c
  | a == c = b

strength :: Port -> Int
strength (Port a b) = a + b

canConnect :: Int -> Port -> Bool
canConnect a (Port b c) = a == b || a == c

buildBridge :: Int -> Pieces -> Set Bridge
buildBridge openPort pieces
  | S.null nextPieces = singleton []
  | otherwise = unions . S.map (nextPiece openPort pieces) $ nextPieces
  where
    nextPieces = S.filter (canConnect openPort) pieces

nextPiece :: Int -> Pieces -> Port -> Set Bridge
nextPiece openPort pieces port =
  S.map (port :) . buildBridge openPort' $ pieces'
  where
    openPort' = nextOpenPort openPort port
    pieces' = delete port pieces

part1 :: Bool -> String -> String
part1 _ =
  show
    . findMax
    . S.map (sum . fmap strength)
    . buildBridge 0
    . fromList
    . fmap makePort
    . numbers

part2 :: Bool -> String -> String
part2 _ =
  show
    . snd
    . findMax
    . S.map (\x -> (length x, sum . fmap strength $ x))
    . buildBridge 0
    . fromList
    . fmap makePort
    . numbers
