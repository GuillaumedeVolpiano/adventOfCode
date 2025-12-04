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
import qualified Data.IntSet as IS (insert, size, filter, member, null, foldr, delete)
import qualified Streamly.Data.Fold as F (foldl')
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Stream as S (fold)
import Data.Bits ((.|.))

purge :: IntSet -> IntSet
purge r = purge' (r, r)
  where
  purge' (rs, toTest)
   | IS.null toTest = rs
   | otherwise = purge' p'
   where
     p' = IS.foldr removeFreeGetNeighbours (rs, mempty) toTest
     removeFreeGetNeighbours p (kept, maybeFreed) =
       case countOccupiedMaybe p 0 [-255, -256, -257, -1, 1, 255, 256, 257] of
         Nothing -> (kept, maybeFreed)
         Just ns -> (IS.delete p kept, foldr IS.insert maybeFreed ns)
     countOccupiedMaybe :: Int -> Int -> [Int] -> Maybe [Int]
     countOccupiedMaybe _ 4 _ = Nothing
     countOccupiedMaybe _ _ [] = Just []
     countOccupiedMaybe p c (x:xs)
       | (p + x) `IS.member` rs = ((p + x) :) <$> countOccupiedMaybe p (c + 1) xs
       | otherwise = countOccupiedMaybe p c xs


removeFree :: IntSet -> IntSet
removeFree rs = IS.filter isNotFree rs
  where
    isNotFree p = countOccupied p 0 [-255, -256, -257, -1, 1, 255, 256, 257]
    countOccupied :: Int -> Int -> [Int] -> Bool
    countOccupied _ 4 _ = True
    countOccupied _ _ [] = False
    countOccupied p c (x:xs) = countOccupied p 
      (if (p + x) `IS.member` rs then c+1 else c) xs
    
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
