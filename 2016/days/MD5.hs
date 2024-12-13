module MD5
  ( md5Concat
  ) where

import           Crypto.Hash.MD5        (hash)
import           Data.ByteString.Base16 (encode)
import           Data.Text              (Text, append)
import           Data.Text.Encoding     (encodeUtf8)

md5Concat :: Text -> Text -> String
md5Concat a b = tail . init . show . encode . hash . encodeUtf8 . append b $ a
