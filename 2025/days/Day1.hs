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
import           Helpers.Streamly.Fold  (readInt)
import qualified Streamly.Data.Fold     as F (foldl', many, one, splitWith,
                                              take, takeEndBy_)
import           Streamly.Data.Fold     (Fold)
import qualified Streamly.Data.Stream   as S (fold)
import           Streamly.Data.Stream   (Stream)

terminators :: [Word8]
terminators = [_space, _lf, _cr]

findPassword :: MonadIO m => ((Int, Int) -> Fold m Int (Int, Int)) -> Fold m Word8 Int
findPassword c = fst <$> F.many (F.takeEndBy_ (`elem` terminators)
  rotation) (c (0,50))

rotation :: MonadIO m => Fold m Word8 Int
rotation = F.splitWith extract (F.take 1 F.one) readInt

extract :: Maybe Word8 -> Int -> Int
extract w v = case w of
                Just 82 -> v
                Just 76 -> negate v
                _       -> error $ "wrong direction: " ++ show w

calc :: Monad m => (Int, Int) -> Fold m Int (Int, Int)
calc = F.foldl' $
  \(count, pos) v ->  let pos' = (pos + v) `mod` 100
                          count' = if pos' == 0 then count + 1
                                                else count
                          in (count', pos')

betterCalc :: Monad m => (Int, Int) -> Fold m Int (Int, Int)
betterCalc = F.foldl' $ \(count, pos) v ->
  let pos' = (pos + v) `mod` 100
      rotations = (pos + v) `div` 100
      delta
        | rotations > 0 = rotations
        | rotations == 0 && pos' == 0 = 1
        | rotations == 0 = 0
        | pos == 0 && pos' == 0 = negate rotations
        | pos == 0 = negate rotations - 1
        | pos' == 0 = negate rotations + 1
        | otherwise = negate rotations
      count' = count + delta
      in (count', pos')


part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = S.fold (findPassword calc) s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = S.fold (findPassword betterCalc) s >>= print
