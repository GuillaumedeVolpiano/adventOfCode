-- |
-- Module      : Helpers.Input
-- Description : The input of a day, and the ways to read it.
--
-- A day is given its whole input as one array of bytes. That is what
-- flatparse and streamly-flatparse work on, and a day that wants a stream
-- opens it itself: the source of the stream is then known where the
-- stream is consumed, which Streamly needs to fuse the two into one loop.
module Helpers.Input
  ( Input
  , bytes
  , chunks
  , byteString
  , fromByteString
  ) where

import           Data.ByteString              (ByteString)
import           Data.Word                    (Word8)
import           Streamly.Data.Array          (Array)
import qualified Streamly.Data.Array          as A (read)
import qualified Streamly.Data.Stream         as S (fromPure)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.External.ByteString as SBS (fromArray, toArray)

-- | The whole input of a day. streamly-flatparse runs a flatparse parser
-- on it as it is (@parseArray@).
type Input = Array Word8

-- | The input byte by byte, for the folds and the parsers of Streamly.
bytes :: Monad m => Input -> Stream m Word8
bytes = A.read
{-# INLINE bytes #-}

-- | The input as a stream of arrays, here a single one: what the stream
-- functions of streamly-flatparse take (@parseMany@, @parseBreak@).
chunks :: Applicative m => Input -> Stream m Input
chunks = S.fromPure
{-# INLINE chunks #-}

-- | The input as a ByteString, without a copy: for flatparse alone.
byteString :: Input -> ByteString
byteString = SBS.fromArray
{-# INLINE byteString #-}

-- | The other way, without a copy either.
fromByteString :: ByteString -> Input
fromByteString = SBS.toArray
{-# INLINE fromByteString #-}
