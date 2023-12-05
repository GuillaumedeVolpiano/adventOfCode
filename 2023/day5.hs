import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           System.TimeIt      (timeIt)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.List          (minimumBy)
import           Data.List.Split    (chunksOf, splitWhen)

parseInput :: [String] -> ([Int], Int -> Int)
parseInput i = (map read . getNumbers . head $ i, getMaps i)

getNumbers :: String -> [String]
getNumbers s = getAllTextMatches (s =~ "[0-9]+") :: [String]

getMaps :: [String] -> (Int -> Int)
getMaps =
  process .
  map (gardenMap . map (truple . map read . getNumbers) . tail) .
  splitWhen null . drop 2

truple :: [Int] -> (Int, Int, Int)
truple [a, b, c] = (a, b, c)

gardenMap :: [(Int, Int, Int)] -> Int -> Int
gardenMap [] x = x
gardenMap ((d, s, l):rs) x
  | x >= s && x - s < l = d + x - s
  | otherwise = gardenMap rs x

ranges :: [Int] -> [(Int, Int)]
ranges []      = []
ranges (a:b:c) = (a, a + b - 1) : ranges c

continuityRanges :: (Int -> Int) -> (Int, Int) -> [(Int, Int)]
continuityRanges f (a, b)
  | f b - f a == b - a = [(a, b)]
  | otherwise = continuityRanges f (a, c) ++ continuityRanges f (c + 1, b)
  where
    c = a + div (b - a) 2

process :: [Int -> Int] -> Int -> Int
process fl x = foldl (\x f -> f x) x fl

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  let (seeds, mapping) = parseInput . lines $ input
  putStrLn "part 1"
  timeIt . print . minimum . map mapping $ seeds
  putStrLn "part 2"
  timeIt .
    print .
    minimum .
    map (\(a, _) -> mapping a) . concatMap (continuityRanges mapping) . ranges $
    seeds
