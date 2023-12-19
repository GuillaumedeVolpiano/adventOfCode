module Day13
  ( part1
  , part2
  ) where

import           Data.List.Split (chunksOf, splitOn, splitWhen)
import qualified Data.Set        as St (Set, findMax, fromList, map, member,
                                        size)
import           Linear.V2       (V2 (..))
import           Text.Regex.TDFA (getAllTextMatches, (=~))

type Pos = V2 Int

type Page = St.Set Pos

type Instruction = (String, Int)

fold :: Instruction -> Page -> Page
fold (dir, val)
  | dir == "x" =
    St.map
      (\(V2 a b) ->
         if a < val
           then V2 a b
           else V2 (2 * val - a) b)
  | dir == "y" =
    St.map
      (\(V2 a b) ->
         if b < val
           then V2 a b
           else V2 a (2 * val - b))

parsePage :: [String] -> Page
parsePage = St.fromList . map ((\[a, b] -> V2 (read a) (read b)) . splitOn ",")

parseInstructions :: [String] -> [Instruction]
parseInstructions = map (\t -> (t =~ "[xy]", read (t =~ "[0-9]+")))

parseInput :: [[String]] -> (Page, [Instruction])
parseInput [a, b] = (parsePage a, parseInstructions b)

oneFold :: (Page, [Instruction]) -> Page
oneFold (page, a:as) = fold a page

foldAll :: (Page, [Instruction]) -> Page
foldAll (page, instructions) = foldl (flip fold) page instructions

render :: Page -> String
render page =
  unlines . chunksOf (mx + 1) $
  [ if V2 x y `St.member` page
    then '#'
    else '.'
  | y <- [0 .. my]
  , x <- [0 .. mx]
  ]
  where
    mx = St.findMax . St.map (\(V2 x _) -> x) $ page
    my = St.findMax . St.map (\(V2 _ y) -> y) $ page

part1 :: Bool -> String -> String
part1 _ = show . St.size . oneFold . parseInput . splitWhen null . lines

part2 :: Bool -> String -> String
part2 _ = render . foldAll . parseInput . splitWhen null . lines
