import           General            (preciseTimeIt, retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

parseLines :: String -> [(Double, Double)]
parseLines = toPairs . map parseLine . lines
  where
    toPairs (a:b:_) = zip a b

parseLine :: String -> [Double]
parseLine l = map read $ getAllTextMatches (l =~ "[0-9]+")

parseOne :: String -> (Double, Double)
parseOne = toPair . map (read . (=~ "[0-9]+")) . lines
  where
    toPair (a:b:_) = (a, b)

-- The problem boils down to x(t - x) > r, that is x² - tx + r < 0,
-- that is any integer between the roots of x^2 - tx + r
quadraticSolution :: (Double, Double) -> Int
quadraticSolution (t, r) = maxX - minX + 1
  where
    minX = ceiling ((t - sqrt (t ^ 2 - 4 * r)) / 2)
    maxX = floor ((t + sqrt (t ^ 2 - 4 * r)) / 2)

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  putStrLn "part 1"
  preciseTimeIt 5 . print . product . map quadraticSolution . parseLines $ input
  putStrLn "part 2"
  preciseTimeIt 6 . print . quadraticSolution . parseOne . filter (/= ' ') $
    input
