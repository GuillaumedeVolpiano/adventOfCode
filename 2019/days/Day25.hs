module Day25
  ( part1
  , part2
  , interaction
  ) where

import           Intcode (Intcode, initialise, runASCIIcode, sendASCIIInput)

runCode :: Intcode -> IO ()
runCode machine = do
  let (output, newMachine) = runASCIIcode machine
  putStrLn output
  inst <- getLine
  runCode . sendASCIIInput (inst ++ "\n") $ newMachine

interaction :: String -> IO ()
interaction = runCode . initialise

part1 :: Bool -> String -> String
part1 _ _ = "This day should be ran interactively"

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
