module Day5
  ( part1
  , part2
  ) where

import           Data.Char              (digitToInt)
import           Data.List              as L (init, isPrefixOf, sort)
import           Data.Text              as T (Text, init)
import           GHC.Conc               (numCapabilities)
import           TextShow               (showt)
import MD5 (md5Concat)

md5Prefix :: [Int] -> Text -> [String]
md5Prefix a b = filter ("00000" `isPrefixOf`) . map (flip md5Concat b . showt) $ a

decode :: [String] -> String
decode = compile [] . map (take 2 . drop 5)

compile :: [(Int, Char)] -> [String] -> String
compile found ([a, b]:xs)
  | length found == 8 = map snd . sort $ found
  | v > 7 || v `elem` map fst found = compile found xs
  | otherwise = compile ((v, b) : found) xs
  where
    v = digitToInt a

part1 :: Bool -> Text -> String
part1 _ = take 8 . map (!! 5) . md5Prefix [0 ..] . T.init

part2 :: Bool -> Text -> String
part2 _ = decode . md5Prefix [0 ..] . T.init
