{-# LANGUAGE BangPatterns #-}
module Day5
  ( part1
  , part2
  , countFresh
  , countAllFresh
  ) where

import           Data.IntMap.Strict   (IntMap)
import qualified Data.IntMap.Strict   as IM (delete, foldrWithKey, insert, keys,
                                             lookupLE)
import           Data.Maybe           (fromJust, isNothing)
import           Data.Word            (Word8)
import           Data.Word8           (_hyphen, _lf)
import qualified Streamly.Data.Fold   as F (foldl')
import qualified Streamly.Data.Stream as S (fold)
import           Streamly.Data.Stream (Stream)

-- | Folding state machine: pper
-- limit, current lower value (or just an value), current higher value, if in
-- the first stage of the fold was the last character we saw a line feed, if in
-- the first stage of the fold was the last character we saw an hyphen or a
-- second linefeed, count of valid values
data FoodRange = FR !(IntMap Int) {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Bool !Bool {-# UNPACK #-} !Int

isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

digit :: Word8 -> Int
digit w = fromIntegral (w - 48)
{-# INLINE digit #-}

insertFR :: FoodRange -> FoodRange
insertFR (FR !lohis !l !h _ _ c)
  | isNothing h' = FR (IM.insert l h lohis) 0 0 True False c
  | isNothing l' = FR (IM.insert l (max h . snd . fromJust $ h') . foldr IM.delete lohis
      . filter (\p -> p > l && p <= h)
      . IM.keys $ lohis) 0 0 True False c
  | l' == h' = if (snd .fromJust $ l') < l then FR (IM.insert l h lohis) 0 0 True False c
                                        else FR (IM.insert (fst . fromJust $ l') (max h . snd . fromJust $ l') lohis) 0 0 True False c
  | otherwise = FR lohis'' 0 0 True False c
  where
    l' = IM.lookupLE l lohis
    h' = IM.lookupLE h lohis
    lohis' = foldr IM.delete lohis . filter (\p -> p > l && p <= h) . IM.keys $ lohis
    lohis'' = if (snd .fromJust $ l') < l then IM.insert l (max h . snd . fromJust $ h') lohis'
                                          else IM.insert (fst . fromJust $ l') (max h . snd . fromJust $ h') lohis'

checkVal :: FoodRange -> FoodRange
checkVal (FR !lohis !l h s t !c)
  | isNothing l' = FR lohis 0 h s t c
  | otherwise = if snd (fromJust l') >= l then FR lohis 0 h s t (c + 1)
                                          else FR lohis 0 h s t c
  where
    l' = IM.lookupLE l lohis
{-# INLINE checkVal #-}

readDB :: FoodRange -> Word8 -> FoodRange
readDB fr@(FR !lohis !l !h !switch !toggle c) w
  | switch && toggle && isDigit w = FR lohis (10 * l + digit w) h True True c
  | switch && toggle && w == _lf = checkVal fr
  | isDigit w = if toggle then FR lohis l (10*h + digit w) False toggle c
                          else FR lohis (10*l + digit w) h False toggle c
  | w == _hyphen = FR lohis l h switch True c
  | w == _lf = if switch then FR lohis l h True True c
                         else insertFR fr
  | otherwise = undefined

shortReadDB :: FoodRange -> Word8 -> FoodRange
shortReadDB fr@(FR !lohis !l !h !switch !toggle c) w
  | switch && toggle = fr
  | isDigit w = if toggle then FR lohis l (10*h + digit w) False toggle c
                          else FR lohis (10*l + digit w) h False toggle c
  | w == _hyphen = FR lohis l h switch True c
  | w == _lf = if switch then FR lohis l h True True c
                         else insertFR fr
  | otherwise = undefined

getCount :: FoodRange -> Int
getCount (FR _ _ _ _ _ !c) = c
{-# INLINE getCount #-}

freshCount :: FoodRange -> Int
freshCount (FR !lohis _ _ _ _ _) = IM.foldrWithKey (\k v c -> c + v - k + 1) 0 lohis
{-# INLINE freshCount #-}

countFresh :: Stream IO Word8 -> IO Int
countFresh s = getCount <$> S.fold (F.foldl' readDB (FR mempty 0 0 False False 0)) s

countAllFresh :: Stream IO Word8 -> IO Int
countAllFresh s = freshCount <$> S.fold (F.foldl' shortReadDB (FR mempty 0 0 False False 0)) s

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = countFresh s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = countAllFresh s >>= print
