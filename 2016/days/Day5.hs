module Day5
  ( part1
  , part2
  ) where

import           Crypto.Hash.MD5             (hash)
import           Data.ByteString.Base16      (encode)
import           Data.Char                   (digitToInt)
import           Data.List                   as L (init, isPrefixOf, sort)
import           Data.Text                   as T (Text, append, init)
import           Data.Text.Encoding          (encodeUtf8)
import           GHC.Conc                    (numCapabilities)
import           TextShow                    (showt)

md5concat :: [Int] -> Text -> [String]
md5concat a b =
  filter ("00000" `isPrefixOf`)
    . map (tail . L.init . show . encode . hash . encodeUtf8 . append b . showt)
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

part1 :: Bool -> Text -> String
part1 _ = take 8 . map (!! 5) . md5concat [0 ..] . T.init

part2 :: Bool -> Text -> String
part2 _ = decode . md5concat [0 ..] . T.init
