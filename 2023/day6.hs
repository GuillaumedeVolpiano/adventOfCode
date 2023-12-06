import           General            (preciseTimeIt)
import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

parseLines :: String -> [(Int, Int)]
parseLines = toPairs . map parseLine . lines
  where
    toPairs (a:b:_) = zip a b

parseLine :: String -> [Int]
parseLine l = map read $ getAllTextMatches (l =~ "[0-9]+")

kern :: [[Int]] -> (Int, Int)
kern = toPair . map (read . concatMap show)
  where
    toPair (a:b:_) = (a, b)

loseFrom :: (Int, Int) -> [Int] -> Int
loseFrom (time, record) =
  length . takeWhile (<= record) . map (\x -> (time - x) * x)

bestOutcomes :: (Int, Int) -> Int
bestOutcomes race@(time, _) = time + 1 - loseFromLeft - loseFromRight
  where
    loseFromLeft = loseFrom race [0 .. time]
    loseFromRight = loseFrom race [time - x | x <- [0 .. time]]

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  putStrLn "part 1"
  -- let's compare timing between the two versions of bestOutcomes in both
  -- situtations, because why not?
  preciseTimeIt 4 . print . product . map bestOutcomes . parseLines $ input
  putStrLn "part 2"
  preciseTimeIt 4 . print . bestOutcomes . kern . map parseLine . lines $ input
