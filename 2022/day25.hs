import           General            (customPreciseTimeIt, retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
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

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  customPreciseTimeIt "part 1 CPU time" 3 .
    print . reverse . encode . sum . map decode . lines $
    input
  customPreciseTimeIt "part 2 CPU time" 3 . print $ "Part 2"
