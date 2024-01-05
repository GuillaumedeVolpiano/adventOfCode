module Day9
  ( part1
  , part2
  ) where

import           Control.Monad.State.Lazy (State, evalState, get, put)
import           Data.List                as L (drop, inits, tails)
import           Data.Maybe               (Maybe (Nothing), fromJust, isJust,
                                           isNothing)
import           Data.Sequence            as S (Seq ((:<|), (:|>)), drop,
                                                elemIndexL, fromList, (><))

type Preamble = Seq Int

type Accum = Seq Int

type XMAS = State (Preamble, Accum, [Int], Int) [Maybe Int]

buildState :: Int -> [Int] -> (Preamble, Accum, [Int], Int)
buildState pl s = (preamble, accum, L.drop pl s, pl)
  where
    rawPreamble = take pl s
    preamble = fromList rawPreamble
    accum =
      fromList . concatMap (\x -> map (x +) . filter (/= x) $ rawPreamble) $
      rawPreamble

findFirstWeakness :: XMAS
findFirstWeakness = do
  (preamble, accum, list, pl) <- get
  let (l:ist) = list
      p :<| reamble = preamble
      newAccum = S.drop (pl - 1) accum >< fmap (l +) reamble
      rest = evalState findFirstWeakness (reamble :|> l, newAccum, ist, pl)
      result
        | null list = []
        | isNothing (elemIndexL l accum) = Just l : rest
        | otherwise = Nothing : rest
  return result

decryptWeakness :: Bool -> [Int] -> Int
decryptWeakness test xmas = maximum results + minimum results
  where
    target =
      fromJust .
      head . filter isJust . evalState findFirstWeakness . buildState pl $
      xmas
    toTest = tails xmas
    results =
      head .
      concatMap
        (filter (\x -> length x > 1 && sum x == target) .
         takeWhile (\x -> sum x <= target) . inits) $
      toTest
    pl
      | test = 5
      | otherwise = 25

part1 :: Bool -> String -> String
part1 test =
  show .
  fromJust .
  head .
  filter isJust . evalState findFirstWeakness . buildState pl . map read . lines
  where
    pl
      | test = 5
      | otherwise = 25

part2 :: Bool -> String -> String
part2 test = show . decryptWeakness test . map read . lines
  where
    pl
      | test = 5
      | otherwise = 25
