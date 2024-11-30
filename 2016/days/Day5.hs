module Day5
  ( part1
  , part2
  ) where

import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack)
import           Data.Char              (digitToInt)
import           Data.List              (isPrefixOf, sort)

md5concat :: [Int] -> String -> [String]
md5concat a b =
  filter ("00000" `isPrefixOf`)
    . map (tail . init . show . encode . hash . pack . (b ++) . show)
    $ a

decode :: [String] -> String
decode = compile [] . map (take 2 . drop 5)

compile :: [(Int, Char)] -> [String] -> String
compile found ([a, b]:xs)
  | length found == 8 = map snd . sort $ found
  | v > 7 || v `elem` map fst found = compile found xs
  | otherwise = compile ((v, b) : found) xs
  where
    v = digitToInt a

part1 :: Bool -> String -> String
part1 _ = take 8 . map (!! 5) . md5concat [0 ..] . init

part2 :: Bool -> String -> String
part2 _ = decode . md5concat [0 ..] . init
