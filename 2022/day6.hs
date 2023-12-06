import           Data.List.Split    (splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.List          (nub)

day = 6

shorten :: Int -> String -> String
shorten size string@(x:xs)
  | length full == length nubbed = drop size string
  | otherwise = shorten size xs
  where
    full = take size string
    nubbed = nub full

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let sequence = init input
  putStrLn "part 1"
  print (length sequence - (length . shorten 4 $ sequence))
  putStrLn "part 2"
  print (length sequence - (length . shorten 14 $ sequence))
