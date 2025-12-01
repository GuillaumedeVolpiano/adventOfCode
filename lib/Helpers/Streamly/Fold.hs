module Helpers.Streamly.Fold (readInt)

where

import Streamly.Data.Fold  (Fold)
import qualified Streamly.Data.Fold as F (foldl')
import Data.Word (Word8)
import qualified Data.Word8 as W (isDigit)

readInt :: Monad m => Fold m Word8 Int
readInt = F.foldl' readInt' 0 
  where
    readInt' acc v
      | W.isDigit v = acc * 10 + fromIntegral v - 48
      | otherwise = error $ show v ++ "is not a digit"
