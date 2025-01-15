{-# LANGUAGE TemplateHaskell #-}

module Day18
  ( part1
  , part2
  ) where

import           Data.Bits                 (shiftL, shiftR, (.&.))
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (unpack)
import qualified Data.ByteString.Char8     as B (lines)
import           Data.Massiv.Array         (Array, Border (Fill), Comp (Seq),
                                            Ix2 ((:.)), Manifest, P, Sz (..),
                                            compute, fromLists', imap,
                                            makeWindowedArray, (!))
import qualified Data.Massiv.Array         as A (map, sum)
import           Data.Massiv.Array.Stencil (Stencil, makeStencil, mapStencil)
import           Data.Massiv.Array.Unsafe  (makeUnsafeStencil)

golStencil :: Bool -> Bool -> Stencil Ix2 Int Int
golStencil test isPart2 =
  makeUnsafeStencil (Sz (3 :. 3)) (1 :. 1) (golPos test isPart2)

{-# INLINE golStencil #-}
golPos :: Bool -> Bool -> Ix2 -> (Ix2 -> Int) -> Int
golPos test isPart2 index get
  | isPart2 && index `elem` [0 :. 0, 0 :. l, l :. 0, l :. l] = 1
  | (get (0 :. 0) == 0 && length ons == 3)
      || (get (0 :. 0) == 1 && length ons `elem` [2, 3]) = 1
  | otherwise = 0
  where
    l
      | test = 5
      | otherwise = 99
    ons =
      filter
        (== 1)
        [get (x :. y) | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]

gol :: Bool -> Bool -> Array P Ix2 Int -> Array P Ix2 Int
gol test isPart2 = compute . mapStencil (Fill 0) (golStencil test isPart2)

part1 :: Bool -> ByteString -> String
part1 test =
  show
    . A.sum
    . (!! 100)
    . iterate (gol test False)
    . compute
    . A.map
        (\x ->
           if x == '#'
             then 1
             else 0)
    . (fromLists' Seq :: ([String] -> Array P Ix2 Char))
    . map unpack
    . B.lines

part2 :: Bool -> ByteString -> String
part2 test =
  show
    . A.sum
    . (!! iterations)
    . iterate (gol test True)
    . compute
    . imap
        (\i x ->
           if i `elem` corners || x == '#'
             then 1
             else 0)
    . (fromLists' Seq :: ([String] -> Array P Ix2 Char))
    . map unpack
    . B.lines
  where
    iterations
      | test = 5
      | otherwise = 100
    l
      | test = 5
      | otherwise = 99
    corners = [x :. y | x <- [0, l], y <- [0, l]]
