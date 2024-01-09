module Day23
  ( part1
  , part2
  ) where

import           Control.Monad       (forM_)
import           Control.Monad.ST    (ST, runST)
import           Data.Char           (digitToInt, intToDigit)
import           Data.List           as L (length)
import           Data.Vector.Mutable as V (STVector, generate, length, read,
                                           write)

type Game s = STVector s Int

findDest :: Int -> [Int] -> Int -> Int
findDest i l mval
  | i > 0 && i `notElem` l = i
  | i > 0 = findDest (i - 1) l mval
  | otherwise = findDest mval l mval

doRound :: Game s -> ST s ()
doRound game = do
  focus <- V.read game 0
  n1 <- V.read game focus
  n2 <- V.read game n1
  n3 <- V.read game n2
  keepn <- V.read game n3
  let dest = findDest (focus - 1) [n1, n2, n3] (V.length game - 1)
  move <- V.read game dest
  write game focus keepn
  write game n3 move
  write game dest n1
  write game 0 keepn

doNRounds :: Int -> Game s -> ST s ()
doNRounds n game = forM_ [1 .. n] (\_ -> doRound game)

toGame :: Int -> [Int] -> ST s (Game s)
toGame nCups list = generate (nCups + 1) longlist
  where
    longlist x
      | x == 0 = head list
      | x == last list && nCups == L.length list = head list
      | x == nCups && x > L.length list = head list
      | x == last list = 10
      | x <= 9 = nextIn list x
      | otherwise = x + 1
    nextIn (a:xs) x
      | a == x = head xs
      | otherwise = nextIn xs x

runGame :: Int -> Int -> [Int] -> [Int]
runGame nRounds nCups list =
  runST $ do
    game <- toGame nCups list
    forM_ [1 .. nRounds] (\_ -> doRound game)
    mapM (V.read game) [0 .. nCups]

buildResult :: [Int] -> String
buildResult list = build list (list !! 1)
  where
    build l x
      | x == 1 = []
      | otherwise = intToDigit x : build l (l !! x)

score :: [Int] -> Int
score game = n2 * n1
  where
    n1 = game !! 1
    n2 = game !! n1

part1 :: Bool -> String -> String
part1 _ = buildResult . runGame 100 9 . map digitToInt . init

part2 :: Bool -> String -> String
part2 _ = show . score . runGame 10000000 1000000 . map digitToInt . init
