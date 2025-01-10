module Day5
  ( part1
  , part2
  ) where

import           Data.Bits          (shiftR, (.&.))
import           Data.List          (foldl', scanl', sort)
import           Data.Maybe         (fromJust)
import           Data.Text          (Text)
import qualified Data.Text          as T (init)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Word          (Word8)
import           GHC.Conc           (numCapabilities)
import           MD5                (md5ConcatTake)
import           Numeric            (showHex)
import           TextShow           (showt)

md5Prefix :: [Int] -> Text -> [[Word8]]
md5Prefix a b =
  filter (all (== 0) . take 5) . map (flip (md5ConcatTake 4) b . showt) $ a

decode :: [[Word8]] -> String
decode =
  foldr (showHex . snd) ""
    . sort
    . head
    . dropWhile ((< 8) . length)
    . scanl' (flip (compile . (\[a, b] -> (a, b)) . take 2 . drop 5)) []

compile :: (Word8, Word8) -> [(Word8, Word8)] -> [(Word8, Word8)]
compile pair@(index, _) found
  | index > 7 || index `elem` map fst found = found
  | otherwise = pair : found

part1 :: Bool -> Text -> String
part1 _ = foldr showHex "" . take 8 . map (!! 5) . md5Prefix [0 ..] . T.init

part2 :: Bool -> Text -> String
part2 _ = decode . md5Prefix [0 ..] . T.init
