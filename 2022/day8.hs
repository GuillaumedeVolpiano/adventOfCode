import           Data.List.Split    (splitOn)
import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.Char          (digitToInt)
import           Data.Map           (Map, fromList, (!))
import           Data.Sequence      (Seq ((:<|), (:|>)))

day = 8

findVisible ::
     [[Int]] -> Map (Int, Int) Bool -> Seq (Int, Int) -> Map (Int, Int) Bool
findVisible aMap treeVis toVisit = treeVis

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let lined = lines input
      width = length . head $ lined
      height = length lined
      visibleInit =
        fromList
          [ ((x, y), (digitToInt (lined !! y !! x), False))
          | x <- [0 .. width]
          , y <- [0 .. height]
          ]
  putStrLn "part 1"
  putStrLn "part 2"
