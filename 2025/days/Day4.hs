module Day4
  ( part1
  , part2
  , mkMap
  , countAccessible
  ) where

import           Streamly.Data.Stream (Stream)
import           Data.Word (Word8)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (insert, foldr, notMember)
import qualified Streamly.Data.Fold as F (foldl')
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Stream as S (fold)

data RollMap = RM {-# UNPACK #-} !Int !IntSet  !IntSet !IntSet {-# UNPACK #-} !Int

countNeighbours :: RollMap -> RollMap
countNeighbours (RM _ top middle bottom c) = RM 0 middle bottom mempty (IS.foldr countN c middle)
  where
    countN p c' = (\res -> if res > (4 :: Int) then c' + 1 else c') $
      foldr (countOcc p) 0 [-1, 0, 1]
    countOcc p v c' = foldr (countOne (p +v)) c' [top, middle, bottom]
    countOne v s c' = if v `IS.notMember` s then c' + 1 else c'
    
countAccessible :: RollMap -> Int
countAccessible = (\(RM _ _ _ _ c') -> c') . countNeighbours 

mapReader :: Monad m => RollMap -> Fold m Word8 RollMap
mapReader = F.foldl' $ \rm@(RM pos top middle bottom c) w -> case w of
                            64 -> RM (pos + 1) top middle (IS.insert pos bottom) c
                            46 -> RM (pos + 1) top middle bottom c
                            10 -> countNeighbours rm
                            _ -> error $ "unexpectd bit" ++ show w

mkMap :: Stream IO Word8 -> IO RollMap 
mkMap = S.fold (mapReader (RM 0 mempty mempty mempty 0))

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = mkMap s >>= print . countAccessible

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print "Part 2"
