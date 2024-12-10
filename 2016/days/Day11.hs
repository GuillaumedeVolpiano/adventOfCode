{-# LANGUAGE TupleSections #-}

module Day11
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (bimap, second)
import           Data.Char            (isAlpha)
import           Data.Either          (fromRight)
import           Data.IntMap          as I (IntMap, empty, fromList, insert,
                                            map, (!))
import           Data.List            as L (filter, null, tails)
import           Data.Maybe           (mapMaybe)
import           Data.Sequence        as Sq (Seq ((:<|), (:|>)), null,
                                             singleton)
import           Data.Set             as S (Set, delete, empty, filter,
                                            fromList, insert, member, notMember,
                                            partition, size, toList)
import           Data.Text            (Text, pack)
import           Helpers.Parsers.Text (Parser, string)
import           Helpers.Search       (bfsDist)
import           Text.Megaparsec      (eof, manyTill, optional, parse,
                                       takeWhileP, try, (<|>))
import           Text.Megaparsec.Char (char, eol)

data Object
  = RTG Text
  | Chip Text
  deriving (Show, Eq, Ord)

type Floors = IntMap (Set Object)

type Elevator = Int

isChip :: Object -> Bool
isChip (Chip _) = True
isChip _        = False

isRTG :: Object -> Bool
isRTG = not . isChip

isSafe :: Object -> Set Object -> Bool
isSafe (Chip material) objects
  | L.null objects = True
  | otherwise = all isChip objects || RTG material `member` objects
isSafe _ _ = True

isSafeFloor :: Set Object -> Bool
isSafeFloor objects = all (`isSafe` objects) objects

parseInput :: Parser Floors
parseInput = I.fromList <$> manyTill parseFloor eof

parseFloor :: Parser (Int, Set Object)
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
  let chips =
        fmap (Chip . snd) . L.filter ((== pack "microchip") . fst) $ contents
      rtgs =
        fmap (RTG . snd) . L.filter ((== pack "generator") . fst) $ contents
  return (floor, S.fromList $ chips ++ rtgs)

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
neighbours pair@(elevator, floors)
  | elevator == 1 = move 2 pair
  | elevator == 4 = move 3 pair
  | all (L.null . (I.!) floors) [1 .. elevator - 1] = move (elevator + 1) pair
  | otherwise = move (elevator + 1) pair ++ move (elevator - 1) pair

move :: Elevator -> (Elevator, Floors) -> [(Elevator, Floors)]
move to state@(from, floors)
  | to < from && not (L.null moved1) = moved1
  | to < from = moved2
  | not (L.null moved2) = moved2
  | otherwise = moved1
  where
    objects = toList $ floors I.! from
    pairs = concat $ zipWith (\a -> fmap (a, )) <*> tails . tail $ objects
    moved2 = mapMaybe (moveTwo to state) pairs
    moved1 = mapMaybe (moveOne to state) objects

moveOne :: Elevator -> (Elevator, Floors) -> Object -> Maybe (Elevator, Floors)
moveOne to (from, floors) object
  | isSafeFloor fromFloor' && isSafeFloor toFloor' = Just (to, floors')
  | otherwise = Nothing
  where
    fromFloor' = delete object $ floors I.! from
    toFloor' = S.insert object $ floors I.! to
    floors' = I.insert to toFloor' . I.insert from fromFloor' $ floors

moveTwo ::
     Elevator
  -> (Elevator, Floors)
  -> (Object, Object)
  -> Maybe (Elevator, Floors)
moveTwo to (from, floors) (o1, o2)
  | isSafeFloor fromFloor' && isSafeFloor toFloor' = Just (to, floors')
  | otherwise = Nothing
  where
    fromFloor' = delete o1 . delete o2 $ floors I.! from
    toFloor' = S.insert o1 . S.insert o2 $ floors I.! to
    floors' = I.insert to toFloor' . I.insert from fromFloor' $ floors

goal :: (Elevator, Floors) -> Bool
goal (elevator, floors) = elevator == 4 && all (L.null . (I.!) floors) [1 .. 3]

preparePart2 :: Floors -> Floors
preparePart2 floors = I.insert 1 firstFloor' floors
  where
    firstFloor = floors I.! 1
    firstFloor' =
      S.insert (RTG . pack $ "elerium")
        . S.insert (Chip . pack $ "elerium")
        . S.insert (RTG . pack $ "dilithium")
        . S.insert (Chip . pack $ "dilithium")
        $ firstFloor

specialBFS ::
     Seq ((Elevator, Floors), Int) -> Set (Int, IntMap (Int, Int)) -> Int
specialBFS toSee seen
  | Sq.null toSee = error "goal not found"
  | goal pos = counter
  | otherwise = specialBFS toSee' seen'
  where
    ((pos, counter) :<| rest) = toSee
    toConsider =
      L.filter (\p -> simplify p `S.notMember` seen) . neighbours $ pos
    toSee' = foldr (flip (:|>) . (, counter + 1)) rest toConsider
    seen' = foldr (S.insert . simplify) seen toConsider
    simplify = second $ I.map (bimap length length . partition isChip)

part1 :: Bool -> Text -> String
part1 _ input = show . specialBFS (singleton ((1, floors), 0)) $ S.empty
  where
    floors = fromRight I.empty . parse parseInput "" $ input

part2 :: Bool -> Text -> String
part2 _ input = show . specialBFS (singleton ((1, floors), 0)) $ S.empty
  where
    floors = preparePart2 . fromRight I.empty . parse parseInput "" $ input
