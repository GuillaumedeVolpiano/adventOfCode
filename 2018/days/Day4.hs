module Day4
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Bifunctor       (second)
import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Data.IntMap          (IntMap, fromList)
import           Data.List            (group, groupBy, maximumBy, sort, sortBy)
import           Data.List.Split      (chunksOf)
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (eof, many, optional, parse, takeWhile1P,
                                       try, (<|>))
import           Text.Megaparsec.Char (char, eol, string)

data Date =
  Date Year Month Day
  deriving (Show, Eq)

type Year = Int

type Month = Int

type Day = Int

data Time =
  Time Hour Minute
  deriving (Show, Eq)

type Hour = Int

type Minute = Int

data Activity
  = ID Int
  | Sleep
  | Wake
  deriving (Show, Eq, Ord)

data Stamp =
  Stamp Time Activity
  deriving (Show, Eq, Ord)

instance Ord Time where
  compare (Time h1 m1) (Time h2 m2) = compare h1 h2 `mappend` compare m1 m2

instance Ord Date where
  compare (Date y1 m1 d1) (Date y2 m2 d2) =
    compare y1 y2 `mappend` compare m1 m2 `mappend` compare d1 d2

next :: Date -> Date
next (Date y m d)
  | m == 12 && d == 31 = Date (y + 1) 1 1
  | d == 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10) =
    Date y (m + 1) 1
  | d == 31 = error ("31st day of " ++ show m)
  | d == 30 && (m == 4 || m == 6 || m == 9 || m == 11) = Date y (m + 1) 1
  | d == 29 && m == 2 && mod y 4 == 0 = Date y (m + 1) 1
  | d == 28 && m == 2 && mod y 4 /= 0 = Date y (m + 1) 1
  | d == 30 && m == 2 = error "February does not have 30 days"
  | d == 29 && m == 2 = error ("February " ++ show y ++ " is not a leap year.")
  | otherwise = Date y m (d + 1)

parser :: Parser [(Date, Stamp)]
parser = many parseLine <* eof

parseLine :: Parser (Date, Stamp)
parseLine = do
  void . char $ '['
  year <- read <$> takeWhile1P Nothing isDigit
  void . char $ '-'
  month <- read <$> takeWhile1P Nothing isDigit
  void . char $ '-'
  day <- read <$> takeWhile1P Nothing isDigit
  void . char $ ' '
  hour <- read <$> takeWhile1P Nothing isDigit
  void . char $ ':'
  minute <- read <$> takeWhile1P Nothing isDigit
  void . string $ "] "
  activity <- try parseShift <|> try parseSleep <|> parseWake
  void . optional $ eol
  let date = Date year month day
      result
        | hour == 23 = (next date, Stamp (Time 0 (-1)) activity)
        | otherwise = (date, Stamp (Time hour minute) activity)
  return result

parseShift :: Parser Activity
parseShift = do
  void . string $ "Guard #"
  iD <- read <$> takeWhile1P Nothing isDigit
  void . string $ " begins shift"
  return (ID iD)

parseSleep :: Parser Activity
parseSleep = do
  void . string $ "falls asleep"
  return Sleep

parseWake :: Parser Activity
parseWake = do
  void . string $ "wakes up"
  return Wake

makeSleepMap :: [(Date, Stamp)] -> [(Int, [[Int]])]
makeSleepMap =
  map (foldr (\(a, b) (_, d) -> (a, b : d)) (0, [])) .
  groupBy (\(a, _) (b, _) -> a == b) .
  sortBy (\(a, _) (b, _) -> compare a b) .
  map
    (makeGuardDay .
     sortBy (\(Stamp t1 _) (Stamp t2 _) -> compare t1 t2) . map snd) .
  groupBy (\(a, _) (b, _) -> a == b) . sortBy (\(a, _) (b, _) -> compare a b)

makeGuardDay :: [Stamp] -> (Int, [Int])
makeGuardDay day = (iD, dets)
  where
    (Stamp _ (ID iD)) = head day
    periods = chunksOf 2 . tail $ day
    dets =
      concatMap
        (\[Stamp (Time _ m1) Sleep, Stamp (Time _ m2) Wake] -> [m1 .. (m2 - 1)])
        periods

maxSleep :: [(Int, [[Int]])] -> Int
maxSleep records = id * minute
  where
    (id, sleeps) =
      maximumBy
        (\(_, a) (_, b) -> compare (sum . map length $ a) (sum . map length $ b))
        records
    minute =
      head .
      maximumBy (\a b -> compare (length a) (length b)) . group . sort . concat $
      sleeps

bestMin :: [(Int, [[Int]])] -> Int
bestMin records = id * minute
  where
    sorted =
      map (second (maximumBy (\a b -> compare (length a) (length b)))) .
      filter (\(_, b) -> not (null b)) . map (second (group . sort . concat)) $
      records
    (id, minute:_) =
      maximumBy (\(_, a) (_, b) -> compare (length a) (length b)) sorted

part1 :: Bool -> String -> String
part1 _ = show . maxSleep . makeSleepMap . fromRight [] . parse parser ""

part2 :: Bool -> String -> String
part2 _ = show . bestMin . makeSleepMap . fromRight [] . parse parser ""
