module Day14
  ( part1
  , part2
  , stretch
  ) where

import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.List              as L (init, isInfixOf)
import           Data.Maybe             (fromJust, isJust)
import           Data.Text              as T (Text, append, init, unpack)
import           Data.Text.Encoding     (encodeUtf8)
import           MD5                    (md5Concat)
import           TextShow               (showt)

showMap :: [Int] -> Text -> [String]
showMap indices salt = map (flip md5Concat salt . showt) indices

findKeys :: Int -> [String] -> [Int]
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

stretch :: Text -> Int -> String
stretch a =
  tail
    . L.init
    . show
    . (!! 2017)
    . iterate (encode . hash)
    . encodeUtf8
    . append a
    . showt

part1 :: Bool -> Text -> String
part1 _ = show . (!! 63) . findKeys 0 . showMap [0 ..] . T.init

part2 :: Bool -> Text -> String
part2 _ = show . (!! 63) . findKeys 0 . flip map [0 ..] . stretch . T.init
