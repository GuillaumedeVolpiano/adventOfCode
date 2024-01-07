module Day16
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Char            (isAlpha, isDigit)
import           Data.Either          (fromRight)
import           Data.HashMap.Strict  as M (HashMap, elems, filter, fromList,
                                            keys)
import           Data.Ix              (inRange)
import           Data.List            as L (filter, isPrefixOf, transpose, (\\))
import           Data.List.Split      (splitOn, splitWhen)
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (parse, takeWhile1P)
import           Text.Megaparsec.Char (char, string)

type State = (FieldDic, Ticket, [Ticket])

type FieldDic = HashMap String ((Int, Int), (Int, Int))

type Ticket = [Int]

buildDic :: [String] -> FieldDic
buildDic =
  fromList . map (fromRight ("", ((0, 0), (0, 0))) . parse parseField "")
  where
    parseField = do
      field <- takeWhile1P Nothing (/= ':')
      void . string $ ": "
      fbfv <- takeNum
      void . char $ '-'
      fbsv <- takeNum
      void . string $ " or "
      sbfv <- takeNum
      void . char $ '-'
      sbsv <- takeNum
      return (field, ((read fbfv, read fbsv), (read sbfv, read sbsv)))
    takeNum = takeWhile1P Nothing isDigit :: Parser String

buildTickets :: [String] -> [Ticket]
buildTickets = map (map read . splitOn ",") . tail

sumInvalids :: State -> Int
sumInvalids (fieldDic, _, tickets) =
  sum . map (sum . L.filter (not . isValid)) $ tickets
  where
    ranges = elems fieldDic
    isValid x = any (isInRange x) ranges

isInRange :: Int -> ((Int, Int), (Int, Int)) -> Bool
isInRange x (a, b) = inRange a x || inRange b x

validsOnly :: State -> State
validsOnly (fieldDic, myTicket, tickets) =
  (fieldDic, myTicket, L.filter (all isValid) tickets)
  where
    isValid x = any (isInRange x) ranges
    ranges = elems fieldDic

identifyRanges :: State -> [String]
identifyRanges (fieldDic, myTicket, tickets) = simplify . map keys $ potDics
  where
    allTickets = transpose $ myTicket : tickets
    potDics =
      map
        (\l -> M.filter (\range -> all (`isInRange` range) l) fieldDic)
        allTickets

simplify :: [[String]] -> [String]
simplify l
  | all ((== 1) . length) l = map head l
  | otherwise = simplify simplified
  where
    singles = map head . L.filter ((== 1) . length) $ l
    simplified = map reduce l
    reduce x
      | length x == 1 = x
      | otherwise = x \\ singles

clarifyTicket :: State -> Int
clarifyTicket state@(_, myTicket, _) = vals
  where
    ids = identifyRanges state
    ided = zip ids myTicket
    vals = product . map snd . L.filter (isPrefixOf "departure" . fst) $ ided

part1 :: Bool -> String -> String
part1 _ =
  show .
  sumInvalids .
  (\[a, b, c] -> (buildDic a, head . buildTickets $ b, buildTickets c)) .
  splitWhen null . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  clarifyTicket .
  validsOnly .
  (\[a, b, c] -> (buildDic a, head . buildTickets $ b, buildTickets c)) .
  splitWhen null . lines
