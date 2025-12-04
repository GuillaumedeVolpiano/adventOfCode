module Day4
  ( part1
  , part2
  , mkMap
  , countAccessible
  , removeFree
  , purge
  ) where

import           Streamly.Data.Stream (Stream)
import           Data.Word (Word8)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (insert, size, filter, member)
import qualified Streamly.Data.Fold as F (foldl')
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Stream as S (fold)
import Data.Bits ((.|.))

purge :: IntSet -> IntSet
purge rs
  | IS.size rs == IS.size rs' = rs
  | otherwise = purge rs'
  where
    rs' = removeFree rs

removeFree :: IntSet -> IntSet
removeFree rs = IS.filter isNotFree rs
  where
    isNotFree p = (>= 4) . length . filter (occupied . (p +)) $ 
      [-255, -256, -257, -1, 1, 255, 256, 257]
    occupied p = p `IS.member` rs
    
countAccessible :: (IntSet -> IntSet) -> IntSet -> Int
countAccessible f rs = IS.size rs - (IS.size . f $ rs)

mapReader :: Monad m => (Int, IntSet) -> Fold m Word8 (Int, IntSet)
mapReader = F.foldl' $ \(pos, rs) w -> case w of
                            64 -> (pos + 1, IS.insert pos rs)
                            46 -> (pos + 1, rs)
                            10 -> ((pos .|. 255) + 1, rs)
                            _ -> error $ "unexpectd bit" ++ show w

mkMap :: Stream IO Word8 -> IO IntSet
mkMap = fmap snd. S.fold (mapReader (0, mempty))

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = mkMap s >>= print . countAccessible removeFree

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = mkMap s >>= print . countAccessible purge
