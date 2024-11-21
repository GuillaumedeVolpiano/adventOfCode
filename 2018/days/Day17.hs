{-# LANGUAGE TupleSections #-}

module Day17
  ( part1
  , part2
  ) where

import           Data.List            (maximumBy, minimumBy, unfoldr)
import           Data.Map             as M (Map, alter, filter, filterWithKey,
                                            fromList, insert, keys, lookup,
                                            notMember, size, update, (!))
import           Helpers.Graph        (Pos)
import           Helpers.Parsers      (Parser, nums, parseByLine)
import           Linear.V2            (V2 (..))
import           Text.Megaparsec      ((<|>))
import           Text.Megaparsec.Char (char, eol, printChar, string)

import           Data.List.Split      (chunksOf)
import           Data.Ord             (comparing)

data Reading
  = Flowing
  | PermFlow
  | Clay
  | Still
  deriving (Show, Eq, Ord)

type Slice = Map Pos Reading

firstFlow = V2 500 1

parseReading :: Parser [(Pos, Reading)]
parseReading = do
  axis <- printChar
  _ <- char '='
  axval <- nums
  _ <- string ", "
  _ <- printChar
  _ <- char '='
  v1 <- nums
  _ <- string ".."
  v2 <- nums
  _ <- eol
  return $ makeLine axis axval v1 v2

makeLine :: Char -> Maybe Int -> Maybe Int -> Maybe Int -> [(Pos, Reading)]
makeLine 'x' (Just x) (Just v1) (Just v2) = map ((, Clay) . V2 x) [v1 .. v2]
makeLine 'y' (Just y) (Just v1) (Just v2) =
  map ((, Clay) . flip V2 y) [v1 .. v2]

flow :: Slice -> Slice
flow slice
  | newSlice == slice = slice
  | otherwise = flow newSlice
  where
    newSlice = foldr (flowOne maxY) slice . keys . M.filter (== Flowing) $ slice
    maxY = maximum . map (\(V2 x y) -> y) . keys . M.filter (== Clay) $ slice

flowOne :: Int -> Pos -> Slice -> Slice
flowOne maxY pos@(V2 x y) slice
  | y == maxY = insert pos PermFlow slice
  | V2 x (y + 1) `notMember` slice = insert (V2 x (y + 1)) Flowing slice
  | slice ! V2 x (y + 1) == PermFlow = insert pos PermFlow slice
  | slice ! V2 x (y + 1) == Flowing = slice
  | otherwise = latFlow pos slice

latFlow :: Pos -> Slice -> Slice
latFlow pos@(V2 x y) slice = flowRight . flowLeft . foldr setState slice $ range
  where
    (minX, isBoundedLeft) = last . unfoldr expandLeft $ (x, False, False)
    (maxX, isBoundedRight) = last . unfoldr expandRight $ (x, False, False)
    range = [minX .. maxX]
    setState p = alter upper (V2 p (y - 1)) . insert (V2 p y) newState
    newState
      | isBoundedLeft && isBoundedRight = Still
      | otherwise = PermFlow
    expandLeft (px, bounded, finished)
      | finished = Nothing
      | M.lookup (V2 px (y + 1)) slice `notElem` [Just Clay, Just Still] =
        Just ((px, False), (px - 1, False, True))
      | M.lookup (V2 (px - 1) y) slice == Just Clay =
        Just ((px, True), (px - 1, True, True))
      | otherwise = Just ((px, False), (px - 1, False, False))
    expandRight (px, bounded, finished)
      | finished = Nothing
      | M.lookup (V2 px (y + 1)) slice `notElem` [Just Clay, Just Still] =
        Just ((px, False), (px + 1, False, True))
      | M.lookup (V2 (px + 1) y) slice == Just Clay =
        Just ((px, True), (px + 1, True, True))
      | otherwise = Just ((px, False), (px + 1, False, False))
    flowRight
      | isBoundedRight = id
      | otherwise = insert (V2 maxX (y + 1)) Flowing
    flowLeft
      | isBoundedLeft = id
      | otherwise = insert (V2 minX (y + 1)) Flowing
    upper p
      | p == Just PermFlow = Just Flowing
      | otherwise = p

render :: Slice -> String
render slice =
  unlines . chunksOf (maxX - minX + 1)
    $ [ rendering . M.lookup (V2 x y) $ slice
      | y <- [minY .. maxY]
      , x <- [minX .. maxX]
      ]
  where
    minX = minimum . map (\(V2 x _) -> x) . keys $ slice
    maxX = maximum . map (\(V2 x _) -> x) . keys $ slice
    minY = minimum . map (\(V2 _ y) -> y) . keys $ slice
    maxY = maximum . map (\(V2 _ y) -> y) . keys $ slice
    rendering (Just Clay)     = '#'
    rendering (Just Still)    = '~'
    rendering (Just Flowing)  = '/'
    rendering (Just PermFlow) = '|'
    rendering _               = '.'

score1 :: Slice -> Int
score1 slice =
  size . filterWithKey (\(V2 _ k) _ -> k >= minY) . M.filter (/= Clay) $ slice
  where
    minY = minimum . map (\(V2 _ y) -> y) . keys . M.filter (== Clay) $ slice

score2 :: Slice -> Int
score2 = size . M.filter (== Still)

part1 :: Bool -> String -> String
part1 _ =
  show
    . score1
    . flow
    . insert firstFlow Flowing
    . fromList
    . concat
    . parseByLine parseReading

part2 :: Bool -> String -> String
part2 _ =
  show
    . score2
    . flow
    . insert firstFlow Flowing
    . fromList
    . concat
    . parseByLine parseReading
