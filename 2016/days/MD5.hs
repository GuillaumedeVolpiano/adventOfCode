module MD5
  ( md5Concat
  ) where

import           Crypto.Hash.MD5    (hash)
import           Data.ByteString    (ByteString)
import           Data.Text          (Text, append)
import qualified Data.Text          as T (unpack)
import           Data.Text.Encoding (encodeUtf8)

md5Concat :: Text -> Text -> ByteString
md5Concat a b = hash . encodeUtf8 . append b $ a
