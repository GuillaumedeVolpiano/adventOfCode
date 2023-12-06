import           Data.List.Split    (chunksOf, splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.Sequence      (Seq ((:<|), (:|>)), empty, index,
                                     singleton, (><))

day = 10

executeAll :: Int -> [String] -> Seq Int
executeAll _ [] = empty
executeAll x (i:is) = result >< executeAll reg is
  where
    result = execute x i
    (_ :|> reg) = result

execute :: Int -> String -> Seq Int
execute x "noop" = singleton x
execute x s      = x :<| x + read (words s !! 1) :<| empty

sprite :: Seq Int -> Int -> Char
sprite seq x
  | index seq (x - 1) - 1 <= mod (x - 1) 40 &&
      mod (x - 1) 40 <= index seq (x - 1) + 1 = '#'
  | otherwise = '.'

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let instructions = lines input
      register = 1 :<| executeAll 1 instructions
      result1 =
        index register 19 * 20 + index register 59 * 60 +
        index register 99 * 100 +
        index register 139 * 140 +
        index register 179 * 180 +
        index register 219 * 220
      message = chunksOf 40 $ [sprite register x | x <- [1 .. 240]]
  putStrLn "part 1"
  print result1
  putStrLn "part 2"
  putStrLn . unlines $ message
