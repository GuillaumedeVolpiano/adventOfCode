module Day4
  ( part1
  , part2
  ) where

import           Crypto.Hash.MD5        (hash)
import           Data.Bits              (shiftR, (.&.))
import           Data.ByteString        (ByteString, append, concat)
import qualified Data.ByteString        as BS (init, take, unpack)
import qualified Data.ByteString.Base16 as BSB (encode)
import qualified Data.Serialize         as S (encode)

md5Concat :: Int -> ByteString -> Int -> Bool
md5Concat zeroSize salt value = all (== 0) . take zeroSize $ md5Hash
  where
    md5Hash =
      concatMap (\i -> [shiftR i 4, i .&. 15])
        . BS.unpack
        . BS.take halfSize
        . hash
        . BSB.encode
        . append salt
        . S.encode
        . show
        $ value
    halfSize = div zeroSize 2 + mod zeroSize 2

findFirst :: Int -> ByteString -> Int
findFirst zeroSize input = head . filter (md5Concat zeroSize input) $ [0 ..]

part1 :: Bool -> ByteString -> String
part1 _ = show . findFirst 5 . BS.init

part2 :: Bool -> ByteString -> String
part2 _ = show . findFirst 6 . BS.init
