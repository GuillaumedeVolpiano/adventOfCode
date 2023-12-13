{-# LANGUAGE PackageImports #-}
module Day1 (part1, part2) where
import           "regex-pcre" Text.Regex.PCRE (getAllTextMatches, (=~))

import           Data.Map                     (fromList, (!))

specialRead =
  fromList
    [ ("on", 1)
    , ("tw", 2)
    , ("thre", 3)
    , ("four", 4)
    , ("fiv", 5)
    , ("six", 6)
    , ("seve", 7)
    , ("eigh", 8)
    , ("nin", 9)
    , ("1", 1)
    , ("2", 2)
    , ("3", 3)
    , ("4", 4)
    , ("5", 5)
    , ("6", 6)
    , ("7", 7)
    , ("8", 8)
    , ("9", 9)
    ]

parseLine :: String -> String -> [String]
parseLine p s = getAllTextMatches (s =~ p)

firstLast :: [String] -> Int
firstLast a = 10 * (specialRead ! head a) + specialRead ! last a

part1 :: Bool -> String -> String
part1 _ = show . sum . map (firstLast . parseLine "[1-9]") . lines

part2 :: Bool -> String -> String
part2 _ = show . 
    sum .
    map
      (firstLast .
       parseLine
         "([0-9]|on(?=e)|tw(?=o)|thre(?=e)|four|fiv(?=e)|six|seve(?=n)|eigh(?=t)|nin(?=e))") .
    lines