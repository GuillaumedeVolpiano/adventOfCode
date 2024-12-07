module Day4
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, (!?))
import           Data.Text          (Text)
import           Helpers.Graph      (Pos, dirs, east, north, south, west)
import           Helpers.Parsers.Text    (arrayFromText)
import           Linear.Vector      ((*^))

type Grid = UArray Pos Char

compass = northeast : northwest : southeast : southwest : dirs

northeast = north + east

northwest = north + west

southeast = south + east

southwest = south + west

readGrid :: Grid -> Pos -> Maybe String -> Maybe String
readGrid grid pos lecture = (:) <$> grid !? pos <*> lecture

findChar :: Char -> Grid -> [Pos]
findChar char = map fst . filter ((== char) . snd) . assocs

findXMAS :: Grid -> Pos -> Int
findXMAS grid p = length . filter (== Just "XMAS") . map readString $ compass
  where
    readString d = foldr (\s -> readGrid grid (p + (s *^ d))) (Just "") [0 .. 3]

findXedMAS :: Grid -> Pos -> Bool
findXedMAS grid p = nesw `elem` mas && nwse `elem` mas
  where
    mas = [Just "MAS", Just "SAM"]
    nesw = foldr (readGrid grid) (Just "") [p + northeast, p, p + southwest]
    nwse = foldr (readGrid grid) (Just "") [p + northwest, p, p + southeast]

findAll :: Grid -> Int
findAll grid = sum . map (findXMAS grid) . findChar 'X' $ grid

findAllXed :: Grid -> Int
findAllXed grid = length . filter (findXedMAS grid) . findChar 'A' $ grid

part1 :: Bool -> Text -> String
part1 _ = show . findAll . arrayFromText

part2 :: Bool -> Text -> String
part2 _ = show . findAllXed . arrayFromText
