module Day22
  ( part1
  , part2
  ) where

import           Data.List       as L (null)
import           Data.List.Split (splitWhen)
import           Data.Sequence   as S (Seq ((:<|), (:|>)), empty, fromList,
                                       length, null, take)
import           Data.Set        as St (Set, empty, insert, member)

type Deck = Seq Int

type Game = (Deck, Deck)

type Rounds = Set Game

playGame :: Game -> Game
playGame (a, b)
  | S.null a || S.null b = (a, b)
  | ca > cb = playGame (ra :|> ca :|> cb, rb)
  | cb > ca = playGame (ra, rb :|> cb :|> ca)
  where
    (ca :<| ra) = a
    (cb :<| rb) = b

playRecursiveGame :: Rounds -> Game -> Game
playRecursiveGame rounds (a, b)
  | S.null a || S.null b = (a, b)
  | (a, b) `member` rounds = (a, S.empty)
  | (ca > S.length ra || cb > S.length rb) && ca > cb = wonA
  | ca > S.length ra || cb > S.length rb = wonB
  | S.null . fst $ subGame = wonB
  | S.null . snd $ subGame = wonA
  where
    wonA = playRecursiveGame (insert (a, b) rounds) winA
    wonB = playRecursiveGame (insert (a, b) rounds) winB
    winA = (ra :|> ca :|> cb, rb)
    winB = (ra, rb :|> cb :|> ca)
    (ca :<| ra) = a
    (cb :<| rb) = b
    subGame = playRecursiveGame St.empty (S.take ca ra, S.take cb rb)

deal :: String -> Game
deal =
  (\[a, b] -> (fromList . map read . tail $ a, fromList . map read . tail $ b)) .
  splitWhen L.null . lines

score :: Game -> Int
score (a, b)
  | S.null a = result b
  | S.null b = result a
  where
    result = snd . foldr (\a (c, acc) -> (c + 1, a * c + acc)) (1, 0)

part1 :: Bool -> String -> String
part1 _ = show . score . playGame . deal

part2 :: Bool -> String -> String
part2 _ = show . score . playRecursiveGame St.empty . deal
