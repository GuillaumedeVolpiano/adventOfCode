module Day5
  ( part1
  , part2
  ) where

import           Data.Bits          (shiftR, (.&.))
import           Data.ByteString    (ByteString, unpack, (!?))
import qualified Data.ByteString    as B (drop, take)
import           Data.List          (foldl', scanl', sort)
import           Data.Maybe         (fromJust)
import           Data.Text          (Text)
import qualified Data.Text          as T (init)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Word          (Word8)
import           GHC.Conc           (numCapabilities)
import           MD5                (md5Concat)
import           Numeric            (showHex)
import           TextShow           (showt)

md5Prefix :: [Int] -> Text -> [ByteString]
md5Prefix a b =
  filter (isValid . unpack . B.take 3) . map (flip md5Concat b . showt) $ a

isValid :: [Word8] -> Bool
isValid [a, b, c] = a == 0 && b == 0 && c < 16

decode :: [ByteString] -> String
decode =
  foldr (showHex . snd) ""
    . sort
    . head
    . dropWhile ((< 8) . length)
    . scanl'
        (flip
           (compile
              . (\[a, b] -> (a .&. 15, shiftR b 4))
              . unpack
              . B.take 2
              . B.drop 2))
        []

compile :: (Word8, Word8) -> [(Word8, Word8)] -> [(Word8, Word8)]
compile pair@(index, _) found
  | index > 7 || index `elem` map fst found = found
  | otherwise = pair : found

part1 :: Bool -> Text -> String
part1 _ =
  foldr showHex ""
    . take 8
    . map ((.&. 15) . fromJust . (!? 2))
    . md5Prefix [0 ..]
    . T.init

part2 :: Bool -> Text -> String
part2 _ = decode . md5Prefix [0 ..] . T.init
