module Day15
  ( part1
  , part2
  ) where

import           Helpers.Parsers

import           Data.Char       (ord)
import           Data.IntMap     as M (IntMap, assocs, empty, insert, lookup)
import           Data.List.Split (splitOn)
import           Data.Maybe      (Maybe (Just), isNothing)
import           Data.Sequence   as Sq (Seq ((:<|), (:|>)), filter, null,
                                        singleton, spanl, tails, (><))
import           Text.Regex.TDFA ((=~))

type Label = String

type Focal = Int

type Procedure = String

type Boxes = IntMap Box

type Box = Seq (Label, Focal)

hash :: String -> Int
hash = foldl (\a b -> mod ((a + ord b) * 17) 256) 0

step :: Boxes -> Procedure -> Boxes
step boxes procedure
  | isNothing potBox && op == "=" = insert box (singleton (label, focal)) boxes
  | isNothing potBox && op == "-" = boxes
  | op == "=" = insert box (insertInBox label focal content) boxes
  | op == "-" = insert box (removeFromBox label content) boxes
  where
    label = procedure =~ "[[:alpha:]]+"
    op = procedure =~ "[=-]"
    focal = read (procedure =~ "[[:digit:]]+")
    box = hash label
    potBox = M.lookup box boxes
    (Just content) = potBox

insertInBox :: Label -> Focal -> Box -> Box
insertInBox label focal box
  | Sq.null box = singleton (label, focal)
  | Sq.null . Sq.filter (\(a, b) -> a == label) $ box = box :|> (label, focal)
  | otherwise = (before :|> (label, focal)) >< after
  where
    (before, _ :<| after) = spanl (\(a, b) -> a /= label) box

removeFromBox :: Label -> Box -> Box
removeFromBox label box
  | Sq.null box = box
  | Sq.null . Sq.filter (\(a, b) -> a == label) $ box = box
  | otherwise = before >< after
  where
    (before, _ :<| after) = spanl (\(a, b) -> a /= label) box

scoreBox :: Box -> Int
scoreBox = foldl (flip $ (+) . sum) 0 . tails . fmap snd

scoreBoxes :: Boxes -> Int
scoreBoxes = sum . map (\(a, b) -> (a + 1) * scoreBox b) . assocs

part1 :: Bool -> String -> String
part1 _ = show . sum . map hash . splitOn "," . init

part2 :: Bool -> String -> String
part2 _ = show . scoreBoxes . foldl step empty . splitOn "," . init
