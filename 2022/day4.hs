import           Data.List.Split    (splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

day = 4

overlapFull :: ((Int, Int), (Int, Int)) -> Bool
overlapFull ((a, b), (c, d)) = (a <= c && b >= d) || (c <= a && d >= b)

overlapPart :: ((Int, Int), (Int, Int)) -> Bool
overlapPart ((a, b), (c, d)) = not (c > b || d < a)

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let pairs =
        map
          ((\(x:y:_) -> (x, y)) .
           map ((\(x:y:_) -> (read x, read y)) . splitOn "-") . splitOn ",") .
        lines $
        input
      overlapF = length . filter overlapFull $ pairs
      overlapP = length . filter overlapPart $ pairs
  putStrLn "part 1"
  print overlapF
  putStrLn "part 2"
  print overlapP
