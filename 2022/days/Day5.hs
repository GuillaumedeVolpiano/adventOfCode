module Day5
  ( part1
  , part2
  ) where

import           Data.List.Split    (chunksOf, splitOn)

import           Data.Char          (digitToInt)
import           Data.IntMap        as I (IntMap, elems, fromList, insert, (!))
import           Data.List          (transpose)
import           Data.Sequence      as S (Seq ((:<|)), fromList, splitAt, (><))

day = 5

newPileFromString :: IntMap String -> String -> IntMap String
newPileFromString pile orders = insert from moved . insert to piled $ pile
  where
    parsedOrders = words orders
    count = read $ parsedOrders !! 1
    from = read $ parsedOrders !! 3
    to = read $ parsedOrders !! 5
    (moved, piled) = move count (pile ! from, pile ! to)
    move 0 (f, t)    = (f, t)
    move x (f:fs, t) = move (x - 1) (fs, f : t)

newPileFromSeq :: IntMap (Seq Char) -> String -> IntMap (Seq Char)
newPileFromSeq pile orders = insert from moved . insert to piled $ pile
  where
    parsedOrders = words orders
    count = read $ parsedOrders !! 1
    from = read $ parsedOrders !! 3
    to = read $ parsedOrders !! 5
    (moving, moved) = S.splitAt count $ pile ! from
    piled = moving >< pile ! to

cm :: String -> [[String]]
cm = map lines . splitOn "\n\n"

initialState = map (filter (/= ' ')) . transpose . map (map (!! 1) . chunksOf 4)

initialStateString = I.fromList . map (\x -> (digitToInt . last $ x, init x))

initialStateSeq =
  I.fromList . map (\x -> (digitToInt . last $ x, S.fromList . init $ x))

part1 :: Bool -> String -> String
part1 _ input =
  show .
  map head .
  elems . foldl newPileFromString (initialStateString . initialState $ crates) $
  movements
  where
    (crates:movements:_) = cm input

part2 :: Bool -> String -> String
part2 _ input =
  show .
  map (\(a :<| _) -> a) .
  elems . foldl newPileFromSeq (initialStateSeq . initialState $ crates) $
  movements
  where
    (crates:movements:_) = cm input
