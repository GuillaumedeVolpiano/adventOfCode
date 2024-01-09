{-# LANGUAGE DataKinds #-}

module Day25
  ( part1
  , part2
  ) where

import           Data.Maybe                              (fromJust)
import           GHC.TypeLits                            (Natural)
import           Math.NumberTheory.Moduli.Class          (Mod, getVal)
import           Math.NumberTheory.Moduli.Multiplicative (discreteLogarithm,
                                                          isMultElement,
                                                          isPrimitiveRoot)
import           Math.NumberTheory.Moduli.Singleton      (CyclicGroup,
                                                          cyclicGroup)

findDiscreteLogarithm :: Int -> Int -> Natural
findDiscreteLogarithm root remainder = discreteLogarithm cg rt rem
  where
    cg = fromJust cyclicGroup :: CyclicGroup Integer 20201227
    rt = fromJust . isPrimitiveRoot cg . fromIntegral $ root
    rem = fromJust . isMultElement . fromIntegral $ remainder

findKey :: [Int] -> Integer
findKey [cpk, dpk] = ek
  where
    dl = findDiscreteLogarithm 7 cpk
    ek = getVal (fromIntegral dpk ^ dl :: Mod 20201227)

part1 :: Bool -> String -> String
part1 _ = show . findKey . map read . lines

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
