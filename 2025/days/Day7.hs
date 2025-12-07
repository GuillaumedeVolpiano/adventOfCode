{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
module Day7
  ( part1
  , part2
  , countSplits
  , countPaths
  ) where

import           Control.Monad                 (void)
import           Data.Vector.Primitive.Mutable (IOVector)
import qualified Data.Vector.Primitive.Mutable as MV (foldr, new, unsafeModify,
                                                      unsafeRead, unsafeWrite)
import           Data.Word                     (Word8)
import qualified Streamly.Data.Fold            as F (foldlM')
import qualified Streamly.Data.Stream          as S (fold)
import           Streamly.Data.Stream          (Stream)

-- | State accumulator used by the folding function.
-- It tracks:
--
--   * the current horizontal position in the input ('Int')
--   * a mutable vector storing path counts ('IOVector Int')
--   * a counter for the number of splits encountered ('Int')
data FoldState = FS {-# UNPACK #-} !Int !(IOVector Int)  {-# UNPACK #-} !Int

-- | Pattern synonym for newline ('\\n').
pattern LF :: Word8
pattern LF = 10

-- | Pattern synonym for empty cell ('.').
pattern Dot :: Word8
pattern Dot = 46

-- | Pattern synonym for start cell ('S').
pattern S :: Word8
pattern S = 83

-- | Pattern synonym for a splitter cell ('^').
pattern Caret :: Word8
pattern Caret = 94

-- | Maximum width of the grid in characters.
-- This must be large enough for the puzzle's line width with at least one empty
-- column on the left of the leftmost splitter and on the right of the rightmost
-- splitter.
maxWidth :: Int
maxWidth = 141 

-- | Folding function that processes one byte at a time and updates
-- the 'FoldState'. The logic is specific to the input representation:
--
--   * 'LF' resets horizontal position on newline
--   * 'Dot' advances the horizontal position
--   * 'S' marks a starting point in the vector
--   * 'Caret' splits counts left/right if present at current position
--
-- Unsafe vector operations rely on puzzle invariants to ensure bounds.
folder :: FoldState -> Word8 -> IO FoldState
folder (FS !p !mv c) !w = case w of
                            LF -> do
                                    pure $ FS 0 mv c
                            Dot -> pure $ FS (p + 1) mv c
                            S -> do
                                    MV.unsafeWrite mv p 1
                                    pure $ FS (p + 1) mv c
                            Caret -> do
                              v <- MV.unsafeRead mv p
                              if v > 0  then do
                                          MV.unsafeWrite mv p 0
                                          MV.unsafeModify mv (+ v) (p - 1)
                                          MV.unsafeModify mv (+ v) (p + 1)
                                          pure $ FS (p + 1) mv (c + 1)
                                        else pure $ FS (p + 1) mv c
                            _ -> undefined

-- | Count the number of split points ('Caret') encountered in the stream.
-- Consumes the stream strictly and mutates a fresh vector during folding.
countSplits :: Stream IO Word8 -> IO Int
countSplits s = do
  mv <- MV.new maxWidth
  (\(FS _ _ c) -> c) <$> S.fold (F.foldlM' folder (pure $ FS 0 mv 0)) s

-- | Count the total number of paths accumulated in the vector after
-- consuming the whole stream.
countPaths :: Stream IO Word8 -> IO Int
countPaths s = do
  mv <- MV.new 141
  void $ S.fold (F.foldlM' folder (pure $ FS 0 mv 0)) s
  MV.foldr (+) 0 mv

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = countSplits s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = countPaths s >>= print
