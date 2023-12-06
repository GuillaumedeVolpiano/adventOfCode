import           Data.List.Split    (chunksOf, splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.Char          (ord)
import           Data.Set           as S (elemAt, fromList, intersection, map)

day = 3

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let rucksacks = lines input
  let result1 =
        sum .
        Prelude.map
          ((\(x, y) ->
              elemAt 0 .
              S.map (\x -> mod (ord x - 96) 58) . intersection (fromList x) $
              fromList y) .
           (\x -> splitAt (div (length x) 2) x)) $
        rucksacks
  let result2 =
        sum .
        Prelude.map
          ((\x -> mod (ord x - 96) 58) .
           elemAt 0 . foldl1 intersection . Prelude.map fromList) .
        chunksOf 3 $
        rucksacks
  putStrLn "part 1"
  print result1
  putStrLn "part 2"
  print result2
