module Day18
  ( part1
  , part2
  ) where

import           Data.Map        (fromList, (!))
import           Data.Sequence   as Sq (Seq ((:<|), (:|>)), length, singleton)
import           Helpers.Parsers (splitOnSpace)
import           Linear.V2       (V2 (..))
import           Linear.Vector   ((*^))

type Pos = V2 Int

up = V2 0 (-1)

down = V2 0 1

right = V2 1 0

left = V2 (-1) 0

charToDir = fromList [('U', up), ('D', down), ('R', right), ('L', left)]

digitToChar = fromList [('0', 'R'), ('1', 'D'), ('2', 'L'), ('3', 'U')]

go :: Pos -> Char -> Int -> Pos
go p c scalar = (+ p) . (scalar *^) . (!) charToDir $ c

polygon :: Pos -> [(Char, Int)] -> Seq Pos
polygon p []               = singleton p
polygon p ((c, scalar):xs) = p :<| go p c scalar `polygon` xs

close :: Seq Pos -> Seq Pos
close s@(x :<| xs) = s :|> x

shoeLace :: Seq Pos -> Int
shoeLace s
  | Sq.length s == 1 = 0
shoeLace ((V2 x1 y1) :<| xs@((V2 x2 y2) :<| _)) =
  (y1 + y2) * (x1 - x2) + abs (x1 - x2) + abs (y1 - y2) + shoeLace xs

adjustedShoeLace :: Seq Pos -> Int
adjustedShoeLace = (+ 1) . flip div 2 . shoeLace

deHex :: String -> (Char, Int)
deHex (_:_:a:b:c:d:e:f:_) = (digitToChar ! f, read ['0', 'x', a, b, c, d, e])

part1 :: Bool -> String -> String
part1 _ =
  show .
  adjustedShoeLace .
  close . polygon (V2 0 0) . map (\((a:_):b:_) -> (a, read b)) . splitOnSpace

part2 :: Bool -> String -> String
part2 _ =
  show .
  adjustedShoeLace .
  close . polygon (V2 0 0) . map (\(_:_:c:_) -> deHex c) . splitOnSpace
