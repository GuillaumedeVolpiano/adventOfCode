module Day4
  ( part1
  , part2
  ) where

import           Crypto.Hash.MD5    (hash)
import           Data.Bits          (shiftR, (.&.))
import qualified Data.ByteString    as BS (take, unpack)
import           Data.Text          (Text, append)
import qualified Data.Text          as T (init)
import           Data.Text.Encoding (encodeUtf8)
import           TextShow           (showt)

md5Concat :: Int -> Text -> Int -> Bool
md5Concat zeroSize salt value = all (== 0) . take zeroSize $ md5Hash
  where
    md5Hash =
      concatMap (\i -> [shiftR i 4, i .&. 15])
        . BS.unpack
        . BS.take halfSize
        . hash
        . encodeUtf8
        . append salt
        . showt
        $ value
    halfSize = div zeroSize 2 + mod zeroSize 2

findFirst :: Int -> Text -> Int
findFirst zeroSize input = head . filter (md5Concat zeroSize input) $ [0 ..]

part1 :: Bool -> Text -> String
part1 _ = show . findFirst 5 . T.init

part2 :: Bool -> Text -> String
part2 _ = show . findFirst 6 . T.init
