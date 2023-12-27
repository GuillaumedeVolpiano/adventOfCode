module Helpers.Maths
  ( solve
  ) where

import           Data.Sequence as Sq (Seq ((:<|), (:|>)), drop, length,
                                      singleton, zipWith)
import           Linear.V4     (V4 (..))

class Dividable a where
  divide :: a -> a -> a

instance Dividable Integer where
  divide = div

instance Dividable Double where
  divide = (/)

reduce :: Num a => Seq (Seq a) -> Seq (Seq a)
reduce s
  | Sq.length s == 1 = s
reduce (e :<| es) = e :<| (reduce . fmap (remove e) $ es)
  where
    remove e1@(a :<| _) e2@(b :<| _) =
        (\(_ :<| xs) -> xs) $
      Sq.zipWith (-) (fmap (a *) e2) (fmap (b *) e1)

result :: (Num a, Dividable a) => Seq (Seq a) -> Seq a
result s
  | Sq.length s == 1 = singleton . divide b $ a
  where
    ((a :<| b :<| _) :<| _) = s
result (x :<| xs) = collapse x (result xs)
  where
    collapse ((a :<| solved) :|> val) res = ra :<| res
      where
        apply s1 s2
          | null s1 && null s2 = 0
        apply (x :<| xs) (y :<| ys) = (x * y) + apply xs ys
        applied = apply solved res
        ra = divide (val - applied) a

solve :: (Num a, Dividable a) => Seq (Seq a) -> Seq a
solve = result . reduce 
