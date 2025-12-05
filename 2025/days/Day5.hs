{-# LANGUAGE BangPatterns #-}
module Day5
  ( part1
  , part2
  , countFresh
  , countAllFresh
  ) where

import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IM (assocs, delete,
                                                     foldrWithKey, insert, keys,
                                                     lookupLE)
import           Data.Maybe                   (fromJust, isNothing)
import           Data.Vector.Unboxed          (Vector)
import qualified Data.Vector.Unboxed          as V (fromList, length,
                                                    unsafeIndex)
import           Data.Word                    (Word8)
import           Data.Word8                   (_hyphen, _lf)
import qualified Streamly.Data.Fold           as F (foldl')
import qualified Streamly.Data.Stream         as S (fold, foldBreak)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.Internal.Data.Fold  as F (foldt')
import           Streamly.Internal.Data.Scanl (Step (Done, Partial))
import Data.Bits (shiftR)

-- | Folding state machine: pper
-- limit, current lower value (or just an value), current higher value, if in
-- the first stage of the fold was the last character we saw a line feed, if in
-- the first stage of the fold was the last character we saw an hyphen or a
-- second linefeed, count of valid values
data FoodRange = FR !(IntMap Int) {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Bool !Bool
data RangeCount = RC !(Vector (Int, Int)) {-# UNPACK #-} !Int {-# UNPACK #-} !Int  {-# UNPACK #-} !Int

isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

digit :: Word8 -> Int
digit w = fromIntegral (w - 48)
{-# INLINE digit #-}

mkRangeCount :: IntMap Int -> RangeCount
mkRangeCount !lohis = RC v (V.length v) 0 0
  where
    v = V.fromList . IM.assocs $ lohis
{-# SCC mkRangeCount #-}

insertFR :: FoodRange -> FoodRange
insertFR (FR !lohis !l !h _ _)
  | isNothing h' = FR (IM.insert l h lohis) 0 0 True False
  | isNothing l' = FR (IM.insert l (max h . snd . fromJust $ h') . foldr IM.delete lohis
      . filter (\p -> p > l && p <= h)
      . IM.keys $ lohis) 0 0 True False
  | l' == h' = if (snd .fromJust $ l') < l then FR (IM.insert l h lohis) 0 0 True False
                                        else FR (IM.insert (fst . fromJust $ l') (max h . snd . fromJust $ l') lohis) 0 0 True False
  | otherwise = FR lohis'' 0 0 True False
  where
    l' = IM.lookupLE l lohis
    h' = IM.lookupLE h lohis
    lohis' = foldr IM.delete lohis . filter (\p -> p > l && p <= h) . IM.keys $ lohis
    lohis'' = if (snd .fromJust $ l') < l then IM.insert l (max h . snd . fromJust $ h') lohis'
                                          else IM.insert (fst . fromJust $ l') (max h . snd . fromJust $ h') lohis'
{-# SCC insertFR #-}

searchIngredient :: RangeCount -> RangeCount
searchIngredient (RC v l n c) 
  | l < fst (V.unsafeIndex v 0) || l > snd (V.unsafeIndex v (l - 1)) = RC v l 0 c
  | otherwise = binarySearch 0 (l - 1)
  where
    binarySearch !lo !hi
      | lo > hi = RC v l 0 c
      | n < a = binarySearch lo (mid - 1)
      | n > b = binarySearch (mid + 1) hi
      | otherwise = RC v l 0 (c + 1)
      where
        mid = (lo + hi) `shiftR` 1
        (a, b) = V.unsafeIndex v mid
{-# SCC searchIngredient #-}

shortReadDB :: FoodRange -> Word8 -> Step FoodRange (IntMap Int)
shortReadDB fr@(FR !lohis !l !h !switch !toggle) w
  | switch && toggle = Done lohis
  | isDigit w = if toggle then Partial $ FR lohis l (10*h + digit w) False toggle
                          else Partial $ FR lohis (10*l + digit w) h False toggle
  | w == _hyphen = Partial $ FR lohis l h switch True
  | w == _lf = if switch then Partial $ FR lohis l h True True
                         else Partial $ insertFR fr
  | otherwise = undefined

readIngredients :: RangeCount -> Word8 -> RangeCount
readIngredients rc@(RC v l n c) w
  | isDigit w = RC v l (10 * n + digit w) c
  | w == _lf = searchIngredient rc
  | otherwise = undefined

freshCount :: IntMap Int -> Int
freshCount !lohis = IM.foldrWithKey (\k v c -> c + v - k + 1) 0 lohis
{-# INLINE freshCount #-}

getCount :: RangeCount -> Int
getCount (RC _ _ _ c) = c
{-# INLINE getCount #-}

extractLoHis :: FoodRange -> IntMap Int
extractLoHis (FR !lohis _ _ _ _) = lohis
{-# INLINE extractLoHis #-}

countFresh :: Stream IO Word8 -> IO Int
countFresh s = S.foldBreak (F.foldt' shortReadDB (Partial $ FR mempty 0 0 False False) extractLoHis) s
  >>= \(res, s') -> getCount <$> S.fold (F.foldl' readIngredients (mkRangeCount res)) s'

countAllFresh :: Stream IO Word8 -> IO Int
countAllFresh s = freshCount <$> S.fold (F.foldt' shortReadDB (Partial $ FR mempty 0 0 False False) extractLoHis) s

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = countFresh s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = countAllFresh s >>= print
