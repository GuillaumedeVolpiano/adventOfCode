{-# LANGUAGE BangPatterns #-}
module Day7
  ( part1
  , part2
  , countSplits
  ) where

import           Streamly.Data.Stream (Stream)
import           Data.Word (Word8)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (insert, delete, member)
import qualified Streamly.Data.Stream as S (fold)
import qualified Streamly.Data.Fold as F (foldl')

data FoldState = FS {-# UNPACK #-} !Int !IntSet {-# UNPACK #-} !Int

folder :: FoldState -> Word8 -> FoldState
folder (FS !p !is c) !w = case w of
                            10 -> FS 0 is c -- newline
                            46 -> FS (p + 1) is c -- '.'
                            83 -> FS (p + 1) (IS.insert p is) c -- 'S'
                            94 -> if p `IS.member` is then FS (p + 1) 
                                                        (IS.insert (p - 1) . IS.insert (p + 1 ) . IS.delete p $ is)
                                                        (c + 1)
                                                      else FS (p + 1) is c
                            _ -> undefined

countSplits :: Stream IO Word8 -> IO Int
countSplits s = (\(FS _ _ c) -> c) <$> S.fold (F.foldl' folder (FS 0 mempty 0)) s

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = countSplits s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print "Part 2"
