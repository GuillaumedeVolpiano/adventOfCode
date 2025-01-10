module MD5
  ( md5Concat
  , md5ConcatTake
  , dedup
  ) where

import           Crypto.Hash.MD5    (hash)
import           Data.Bits          (shiftR, (.&.))
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS (take, unpack)
import           Data.Text          (Text, append)
import qualified Data.Text          as T (unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word          (Word8)

md5ConcatInternal :: Text -> Text -> ByteString
md5ConcatInternal a b = hash . encodeUtf8 . append b $ a

md5Concat :: Text -> Text -> [Word8]
md5Concat a = dedup . BS.unpack . md5ConcatInternal a

md5ConcatTake :: Int -> Text -> Text -> [Word8]
md5ConcatTake n a = dedup . BS.unpack . BS.take n . md5ConcatInternal a

dedup :: [Word8] -> [Word8]
dedup = concatMap (\i -> [shiftR i 4, i .&. 15])
