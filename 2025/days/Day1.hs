{-# LANGUAGE OverloadedStrings #-}
module Day1
  ( part1
  , part2
  , findPassword
  , calc
  , betterCalc
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Word              (Word8)
import           Data.Word8             (_cr, _lf, _space)
import qualified Streamly.Data.Fold     as F (foldl')
import           Streamly.Data.Fold     (Fold)
import qualified Streamly.Data.Stream   as S (fold)
import           Streamly.Data.Stream   (Stream)
import Data.Maybe (fromMaybe)

terminators :: [Word8]
terminators = [_space, _lf, _cr]


findPassword :: MonadIO m => ((Int, Int) -> Int -> (Int, Int)) -> Fold m Word8 Int
findPassword c = (\(count, _, _, _) -> count) <$> F.foldl' (rotation c) (0, 50, Nothing, 0)

rotation :: ((Int, Int) -> Int -> (Int, Int)) -> (Int, Int, Maybe (Int -> Int), Int) -> Word8 -> (Int, Int, Maybe (Int -> Int), Int)
rotation c (count, pos, dir, n) w
      | w == 82 = (count, pos, Just id, n)
      | w == 76 = (count, pos, Just negate, n)
      | w >= 48 && w <= 57 = (count, pos, dir, 10*n + fromIntegral w - 48)
      | w `elem` terminators = (count', pos', Nothing, 0)
      | otherwise = error $ "unexpected byte" ++ show w
          where
            (count', pos') = c (count, pos) $ fromMaybe (error "No direction found") dir n

calc :: (Int, Int) -> Int -> (Int, Int)
calc (count, pos) v = (count', pos')
  where
    pos' = (pos + v) `mod` 100
    count' = if pos' == 0 then count + 1
                          else count

betterCalc :: (Int, Int) -> Int -> (Int, Int)
betterCalc (count, pos) v = (count', pos') 
  where
   total = pos + v
   pos' = total `mod` 100
   rotations = total `div` 100
   delta
     | rotations <= 0 && pos' == 0 = 1
     | rotations < 0 && pos == 0 = - 1
     | otherwise = 0
   count' = count + abs rotations + delta

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = S.fold (findPassword calc) s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = S.fold (findPassword betterCalc) s >>= print
