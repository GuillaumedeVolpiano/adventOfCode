module Day25 (part1, part2) where
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Maybe         (fromJust)

snafuToDigit = [('2', 2), ('1', 1), ('0', 0), ('-', -1), ('=', -2)]

digitToSnaFu = [(0, '='), (1, '-'), (2, '0'), (3, '1'), (4, '2')]

decode :: String -> Int
decode s =
  sum .
  map (\(a, b) -> (fromJust . lookup a $ snafuToDigit) * 5 ^ b) .
  reverse . zip (reverse s) $
  [0 ..]

encode :: Int -> String
encode s
  | div (s + 2) 5 == 0 = [snafu]
  | otherwise = snafu : encode (div (s + 2) 5)
  where
    cur = mod (s + 2) 5
    snafu = fromJust . lookup cur $ digitToSnaFu

part1 :: Bool -> String -> String
part1 _ = reverse . encode . sum . map decode . lines

part2 _ _ = "Free star"
