module Day12
  ( part1
  , part2
  ) where

import           Data.Char       (isLower, isUpper)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, alter, empty, (!))
import           Data.Maybe      (Maybe (Just, Nothing))
import           Data.Sequence   as Sq (Seq ((:<|), (:|>)), null, singleton)
import           Data.Set        as St (Set, insert, member, singleton)

data State =
  State
    { pos   :: Cave
    , seen  :: Set Cave
    , twice :: Bool
    }

type Cave = String

type System = Map Cave [Cave]

startPos = State "start" (St.singleton "start") False

tuple :: [Cave] -> (Cave, Cave)
tuple [a, b] = (a, b)

toDict :: (Cave, Cave) -> Map Cave [Cave] -> Map Cave [Cave]
toDict (a, b) = alter (toList b) a . alter (toList a) b

toList :: String -> Maybe [Cave] -> Maybe [Cave]
toList a Nothing  = Just [a]
toList a (Just l) = Just (a : l)

explore :: Seq State -> [State] -> System -> Int
explore s fullPaths _
  | Sq.null s = length fullPaths
explore (a :<| as) fullPaths system
  | pos a == "end" = explore as (a : fullPaths) system
  | otherwise = explore newS fullPaths system
  where
    toSee
      | twice a =
        map (\p -> State p (St.insert p $ seen a) True) .
        filter (\p@(f:_) -> isUpper f || (not . member p $ seen a)) $
        system ! pos a
      | otherwise =
        map (twiceState a) . filter (`notElem` ["start"]) $ system ! pos a
    newS = foldl (:|>) as toSee

twiceState :: State -> Cave -> State
twiceState (State p seen False) cave@(c:_)
  | isLower c && cave `member` seen = State cave (insert p seen) True
  | otherwise = State cave (insert p seen) False

part1 :: Bool -> String -> String
part1 _ =
  show .
  explore (Sq.singleton startPos {twice = True}) [] .
  foldr (toDict . tuple . splitOn "-") empty . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  explore (Sq.singleton startPos) [] .
  foldr (toDict . tuple . splitOn "-") empty . lines
