module Day8
  ( part1
  , part2
  ) where

import           Data.Maybe      (Maybe (Just, Nothing), fromJust, isNothing)
import           Data.Set        (Set, empty, insert, member)
import           Data.Vector     (Vector, fromList, (!), (!?))
import           Helpers.Parsers

data BootCode =
  BootCode
    { instructions :: Instructions
    , vals         :: Vals
    , position     :: Position
    , accumulator  :: Int
    , seen         :: Set Position
    }
  deriving (Show)

type Instructions = Vector String

type Vals = Vector Int

type Position = Int

makeBootCode :: [(String, Int)] -> BootCode
makeBootCode inst =
  BootCode (fromList . map fst $ inst) (fromList . map snd $ inst) 0 0 empty

stripPlus :: String -> Int
stripPlus n@(a:as)
  | a == '+' = read as
  | otherwise = read n

advance :: Int -> BootCode -> BootCode
advance d bc =
  bc {position = position bc + d, seen = insert (position bc) $ seen bc}

accumulate :: BootCode -> BootCode
accumulate bc =
  advance 1 $ bc {accumulator = accumulator bc + vals bc ! position bc}

jump :: BootCode -> BootCode
jump bc = advance (vals bc ! position bc) bc

nop :: BootCode -> BootCode
nop = advance 1

curInst :: BootCode -> String
curInst bc = instructions bc ! position bc

loop :: BootCode -> Int
loop bc
  | position bc `elem` seen bc = accumulator bc
  | ci == "acc" = loop . accumulate $ bc
  | ci == "jmp" = loop . jump $ bc
  | ci == "nop" = loop . nop $ bc
  where
    ci = curInst bc

terminate :: BootCode -> Int
terminate bc
  | ci == "acc" = terminate . accumulate $ bc
  | ci == "jmp" && isNothing changedJump = terminate . jump $ bc
  | ci == "jmp" = fromJust changedJump
  | ci == "nop" && isNothing changedNop = terminate . nop $ bc
  | ci == "nop" = fromJust changedNop
  where
    ci = curInst bc
    changedJump = changed . nop $ bc
    changedNop = changed . jump $ bc

changed :: BootCode -> Maybe Int
changed bc
  | position bc `elem` seen bc = Nothing
  | isNothing (instructions bc !? position bc) = Just $ accumulator bc
  | ci == "acc" = changed . accumulate $ bc
  | ci == "jmp" = changed . jump $ bc
  | ci == "nop" = changed . nop $ bc
  where
    ci = curInst bc

part1 :: Bool -> String -> String
part1 _ =
  show .
  loop . makeBootCode . map ((\[a, b] -> (a, stripPlus b)) . words) . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  terminate . makeBootCode . map ((\[a, b] -> (a, stripPlus b)) . words) . lines
