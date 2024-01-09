module Intcode
  ( initialise
  , runIntcode
  , update
  ) where

import           Control.Monad.State (State, evalState, gets, modify, put)
import           Data.IntMap         (IntMap, fromList, insert, (!))
import           Data.List.Split     (splitOn)

type Machine = State Intcode Int

data Intcode =
  Intcode
    { memory  :: IntMap Int
    , pointer :: Int
    }
  deriving (Show, Eq)

runIntcode :: Intcode -> Int
runIntcode = evalState execute

execute :: Machine
execute = do
  pos <- gets pointer
  mem <- gets memory
  let op
        | mem ! pos == 1 = add
        | mem ! pos == 2 = mult
        | mem ! pos == 99 = end
  op

add :: Machine
add = do
  pos <- gets pointer
  mem <- gets memory
  let v1 = mem ! (mem ! (pos + 1))
      v2 = mem ! (mem ! (pos + 2))
      np = mem ! (pos + 3)
  modify (update np (v1 + v2))
  modify (movePos (pos + 4))
  execute

mult :: Machine
mult = do
  pos <- gets pointer
  mem <- gets memory
  let v1 = mem ! (mem ! (pos + 1))
      v2 = mem ! (mem ! (pos + 2))
      np = mem ! (pos + 3)
  modify (update np (v1 * v2))
  modify (movePos (pos + 4))
  execute

end :: Machine
end = do
  mem <- gets memory
  return (mem ! 0)

update :: Int -> Int -> Intcode -> Intcode
update pos val intcode = intcode {memory = insert pos val . memory $ intcode}

movePos :: Int -> Intcode -> Intcode
movePos pos intcode = intcode {pointer = pos}

initialise :: String -> Intcode
initialise instructions = Intcode (fromList (zip [0 ..] parsed)) 0
  where
    parsed = map read . splitOn "," . filter (/= '\n') $ instructions
