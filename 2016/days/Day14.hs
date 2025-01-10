module Day14
  ( part1
  , part2
  , stretch
  ) where

import           Crypto.Hash.MD5        (hash)
import           Data.Bits              (shiftR, (.&.))
import qualified Data.ByteString        as BS (unpack)
import           Data.ByteString.Base16 (encode)
import           Data.List              as L (init, isInfixOf)
import           Data.Maybe             (fromJust, isJust)
import           Data.Text              (Text, append)
import qualified Data.Text              as T (init)
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Data.Word              (Word8)
import           MD5                    (md5Concat)
import           Numeric                (showHex)
import           TextShow               (showt)

saltHash :: Text -> Int -> [Word8]
saltHash salt = dedup . BS.unpack . flip md5Concat salt . showt

dedup :: [Word8] -> [Word8]
dedup = concatMap (\i -> [shiftR i 4, i .&. 15])

findKeys :: Int -> [[Word8]] -> [Int]
findKeys index (h:ashes)
  | isKey = index : findKeys (index + 1) ashes
  | otherwise = findKeys (index + 1) ashes
  where
    isKey =
      isJust (findThree h)
        && any
             (replicate 5 (fromJust . findThree $ h) `isInfixOf`)
             (take 1000 ashes)
    findThree [a, b] = Nothing
    findThree (a:xs@(b:c:_))
      | a == b && a == c = Just a
      | otherwise = findThree xs

stretch :: Text -> Int -> [Word8]
stretch a =
  dedup
    . BS.unpack
    . (!! 2016)
    . iterate (hash . encode)
    . hash
    . encodeUtf8
    . append a
    . showt

part1 :: Bool -> Text -> String
part1 _ = show . (!! 63) . findKeys 0 . flip map [0 ..] . saltHash . T.init

part2 :: Bool -> Text -> String
part2 _ = show . (!! 63) . findKeys 0 . flip map [0 ..] . stretch . T.init
