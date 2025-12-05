{-# LANGUAGE BangPatterns #-}
module Day5
  ( part1
  , part2
  , countFresh
  , countAllFresh
  ) where

import           Control.Monad                 (void)
import           Data.Bits                     (shiftR)
import           Data.Complex                  (Complex ((:+)))
import           Data.IntMap.Strict            (IntMap)
import qualified Data.IntMap.Strict            as IM (delete, foldrWithKey,
                                                      insert, keys, lookupLE,
                                                      size)
import           Data.Maybe                    (fromJust, isNothing)
import           Data.Vector.Primitive.Mutable (IOVector)
import qualified Data.Vector.Primitive.Mutable as MV (new, unsafeRead,
                                                      unsafeWrite)
import           Data.Word                     (Word8)
import           Data.Word8                    (_hyphen, _lf)
import qualified Streamly.Data.Fold            as F (foldlM')
import qualified Streamly.Data.Stream          as S (fold, foldBreak)
import           Streamly.Data.Stream          (Stream)
import qualified Streamly.Internal.Data.Fold   as F (foldt')
import           Streamly.Internal.Data.Fold   (Step (Done, Partial))

-- | Folding state machine: pper
-- limit, current lower value (or just an value), current higher value, if in
-- the first stage of the fold was the last character we saw a line feed, if in
-- the first stage of the fold was the last character we saw an hyphen or a
-- second linefeed, count of valid values
data FoodRange = FR !(IntMap Int) {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Bool !Bool
data RangeCount = RC !(IOVector (Complex Int)) {-# UNPACK #-} !Int {-# UNPACK #-} !Int  {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
newtype Build = Build (Int -> IO Int)

isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

digit :: Word8 -> Int
digit w = fromIntegral (w - 48)
{-# INLINE digit #-}

mkRangeCount :: IntMap Int -> IO RangeCount
mkRangeCount !lohis = do
  let l = IM.size lohis
  v <- MV.new l
  let pass key val (Build k) = Build $ \i -> do
                                  MV.unsafeWrite v i (key :+ val)
                                  k (i + 1)
      Build fd = IM.foldrWithKey pass (Build pure) lohis
  void $ fd 0
  (lo :+ _) <- MV.unsafeRead v 0
  (_ :+ hi) <- MV.unsafeRead v (l - 1)
  pure $ RC v lo hi l 0 0
{-# INLINE mkRangeCount #-}
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

searchIngredient :: RangeCount -> IO RangeCount
searchIngredient (RC !v !lo !hi !l !n !c)
  | n < lo || n > hi = pure $ RC v lo hi l 0 c
  | otherwise = binarySearch 0 (l - 1)
  where
    binarySearch !lv !hv
        | lv > hv = pure $ RC v lo hi l 0 c
        | otherwise = do
            let mid = (lv + hv) `shiftR` 1
            (a :+ b) <- MV.unsafeRead v mid
            let bs
                  | n < a = binarySearch lv (mid - 1)
                  | n > b = binarySearch (mid + 1) hv
                  | otherwise = pure $ RC v lo hi l 0 (c + 1)
            bs
{-# INLINE searchIngredient #-}
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

readIngredients :: RangeCount -> Word8 -> IO RangeCount
readIngredients rc@(RC v lo hi l n c) w
  | isDigit w = pure $ RC v lo hi l (10 * n + digit w) c
  | w == _lf = searchIngredient rc
  | otherwise = undefined
{-# INLINE readIngredients #-}

freshCount :: IntMap Int -> Int
freshCount !lohis = IM.foldrWithKey (\k v c -> c + v - k + 1) 0 lohis
{-# INLINE freshCount #-}

getCount :: RangeCount -> Int
getCount (RC _ _ _ _ _ c) = c
{-# INLINE getCount #-}

extractLoHis :: FoodRange -> IntMap Int
extractLoHis (FR !lohis _ _ _ _) = lohis
{-# INLINE extractLoHis #-}

countFresh :: Stream IO Word8 -> IO Int
countFresh s = S.foldBreak (F.foldt' shortReadDB (Partial $ FR mempty 0 0 False False) extractLoHis) s
  >>= \(res, s') -> getCount <$> S.fold (F.foldlM' readIngredients (mkRangeCount res)) s'

countAllFresh :: Stream IO Word8 -> IO Int
countAllFresh s = freshCount <$> S.fold (F.foldt' shortReadDB (Partial $ FR mempty 0 0 False False) extractLoHis) s

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = countFresh s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = countAllFresh s >>= print
