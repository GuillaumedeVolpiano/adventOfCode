module Day6 (part1, part2) where
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

part1 :: Bool -> String -> String
part1 _ = show . product . map quadraticSolution . parseLines

part2 :: Bool -> String -> String
part2 _ = show . quadraticSolution . parseOne . filter (/= ' ')
