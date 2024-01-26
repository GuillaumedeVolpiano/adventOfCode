module Day10
  ( part1
  , part2
  ) where

import           Control.Lens.Getter ((^.))
import           Data.List           (unfoldr)
import           Data.List.Split     (chunksOf)
import           Helpers.Graph       (Pos)
import           Helpers.Parsers     (integers)
import           Linear.V2           (V2 (..), _x, _y)

findMessage :: [(Pos, Pos)] -> Maybe ([Pos], [(Pos, Pos)])
findMessage curState
  | area newPos > area (map fst curState) = Nothing
  | otherwise = Just (newPos, newState)
  where
    newState = map (\(a, b) -> (a + b, b)) curState
    newPos = map fst newState

area :: [Pos] -> Int
area pos = (mX - mx) * (mY - my)
  where
    ((mx, mX), (my, mY)) = bounds pos

bounds :: [Pos] -> ((Int, Int), (Int, Int))
bounds pos = ((mx, mX), (my, mY))
  where
    mx = minimum . map (^. _x) $ pos
    mX = maximum . map (^. _x) $ pos
    my = minimum . map (^. _y) $ pos
    mY = maximum . map (^. _y) $ pos

render :: [Pos] -> String
render pos =
  unlines . chunksOf (mX - mx + 1) $
  [ if V2 x y `elem` pos
    then '#'
    else ' '
  | y <- [my .. mY]
  , x <- [mx .. mX]
  ]
  where
    ((mx, mX), (my, mY)) = bounds pos

message :: String -> [[Pos]]
message =
  unfoldr findMessage . map (\[a, b, c, d] -> (V2 a b, V2 c d)) . integers

part1 :: Bool -> String -> String
part1 _ = render . last . message

part2 :: Bool -> String -> String
part2 _ = show . length . message
