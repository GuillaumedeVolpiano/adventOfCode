module Day11
  ( part1
  , part2
  ) where

import           Data.Char            (isAlpha)
import           Data.Either          (fromRight)
import           Data.IntMap          (IntMap, empty, fromList, insert, (!))
import           Data.List            (delete, sort)
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text, pack)
import           Helpers.Parsers.Text (Parser, string)
import           Helpers.Search       (bfsDist)
import           Text.Megaparsec      (eof, manyTill, optional, parse,
                                       takeWhileP, try, (<|>))
import           Text.Megaparsec.Char (char, eol)

newtype RTG =
  RTG Text
  deriving (Show, Eq, Ord)

newtype Chip =
  Chip Text
  deriving (Show, Eq, Ord)

type Floors = IntMap ([Chip], [RTG])

type Elevator = Int

isSafe :: Chip -> [RTG] -> Bool
isSafe (Chip material) rtgs
  | null rtgs = True
  | otherwise = RTG material `elem` rtgs

isEmpty :: ([Chip], [RTG]) -> Bool
isEmpty (a, b) = null a && null b

isSafeFloor :: ([Chip], [RTG]) -> Bool
isSafeFloor (chips, rtgs) = all (`isSafe` rtgs) chips

parseInput :: Parser Floors
parseInput = fromList <$> manyTill parseFloor eof

parseFloor :: Parser (Int, ([Chip], [RTG]))
parseFloor = do
  string "The "
  floor <-
    (do
       string "first "
       return 1)
      <|> (do
             string "second "
             return 2)
      <|> (do
             string "third "
             return 3)
      <|> (do
             string "fourth "
             return 4)
  string "floor contains "
  contents <- nothing <|> manyTill parseContents (char '.')
  optional eol
  let chips = map (Chip . snd) . filter ((== pack "microchip") . fst) $ contents
      rtgs = map (RTG . snd) . filter ((== pack "generator") . fst) $ contents
  return (floor, (chips, rtgs))

nothing :: Parser [(Text, Text)]
nothing = do
  string "nothing relevant."
  return []

parseContents :: Parser (Text, Text)
parseContents = do
  optional . char $ ','
  optional . char $ ' '
  optional . string $ "and "
  string "a "
  material <- takeWhileP Nothing isAlpha
  optional . string $ "-compatible"
  char ' '
  genChip <- takeWhileP Nothing isAlpha
  return (genChip, material)

neighbours :: (Elevator, Floors) -> [(Elevator, Floors)]
neighbours pair@(elevator, _)
  | elevator == 1 = up pair
  | elevator == 4 = down pair
  | otherwise =
    up pair
      ++ down pair

up :: (Elevator, Floors) -> [(Elevator, Floors)]
up cur@(from, _) = move to cur
  where
    to = from + 1

down :: (Elevator, Floors) -> [(Elevator, Floors)]
down cur@(from, _) = move to cur
  where
    to = from - 1

move :: Elevator -> (Elevator, Floors) -> [(Elevator, Floors)]
move to (from, floors) =
  oneMoveChip ++ twoMoveChip ++ oneMoveRTG ++ twoMoveRTG ++ movedPaired
  where
    (chips, rtgs) = floors ! to
    oneMoveChip = mapMaybe (moveChip from to floors) chips
    oneMoveRTG = mapMaybe (moveRTG from to floors) rtgs
    twoMoveChip = concatMap (moveSecondChip . snd) oneMoveChip
    twoMoveRTG = concatMap (moveSecondRTG . snd) oneMoveRTG
    moveSecondChip f = mapMaybe (moveChip from to f) . fst . (!) f $ from
    moveSecondRTG f = mapMaybe (moveRTG from to f) . snd . (!) f $ from
    pairedChips = sort . filter (\(Chip m) -> RTG m `elem` rtgs) $ chips
    pairedRTGs = sort . filter (\(RTG m) -> Chip m `elem` chips) $ rtgs
    paired = zip pairedChips pairedRTGs
    movedPaired = map (movePair from to floors) paired

moveChip :: Elevator -> Elevator -> Floors -> Chip -> Maybe (Elevator, Floors)
moveChip from to floors chip
  | isSafeFloor fromFloor' && isSafeFloor toFloor' =
    Just (to, insert to toFloor' . insert from fromFloor' $ floors)
  | otherwise = Nothing
  where
    (fromChips, fromRTGs) = floors ! from
    (toChips, toRTGs) = floors ! to
    fromFloor' = (delete chip fromChips, fromRTGs)
    toFloor' = (chip : toChips, toRTGs)

moveRTG :: Elevator -> Elevator -> Floors -> RTG -> Maybe (Elevator, Floors)
moveRTG from to floors rtg
  | isSafeFloor fromFloor' && isSafeFloor toFloor' =
    Just (to, insert to toFloor' . insert from fromFloor' $ floors)
  | otherwise = Nothing
  where
    (fromChips, fromRTGs) = floors ! from
    (toChips, toRTGs) = floors ! to
    fromFloor' = (fromChips, delete rtg fromRTGs)
    toFloor' = (toChips, rtg : toRTGs)

movePair :: Elevator -> Elevator -> Floors -> (Chip, RTG) -> (Elevator, Floors)
movePair from to floors (chip, rtg) =
  (to, insert to toFloor' . insert from fromFloor' $ floors)
  where
    (fromChips, fromRTGs) = floors ! from
    (toChips, toRTGs) = floors ! to
    fromFloor' = (delete chip fromChips, delete rtg fromRTGs)
    toFloor' = (chip : toChips, rtg : toRTGs)

goal :: (Elevator, Floors) -> Bool
goal (elevator, floors) = elevator == 4 && all (isEmpty . (!) floors) [1 .. 3]

part1 :: Bool -> Text -> String
part1 _ input = show . bfsDist (1, floors) neighbours $ goal
  where
    floors = fromRight empty . parse parseInput "" $ input

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
