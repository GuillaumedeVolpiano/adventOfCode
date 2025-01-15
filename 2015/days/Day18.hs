{-# LANGUAGE TemplateHaskell #-}

module Day18
  ( part1
  , part2
  ) where

import           Data.Bits                 (shiftL, shiftR, (.&.))
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (unpack)
import qualified Data.ByteString.Char8     as B (lines)
import           Data.IntSet               (IntSet, insert, member, size)
import qualified Data.IntSet               as S (foldr)
import           Data.Massiv.Array         (Array, Border (Fill), Comp (Seq),
                                            Ix2 ((:.)), Manifest, P, Sz (..),
                                            compute, fromLists',
                                            makeWindowedArray, (!))
import qualified Data.Massiv.Array         as A (map, sum)
import           Data.Massiv.Array.Stencil (Stencil, makeStencil, mapStencil)
import           Data.Massiv.Array.Unsafe  (makeUnsafeStencil)
import           Data.Maybe                (fromJust)
import           Data.Void                 (Void)
import           FlatParse.Stateful        (Result (OK), eof, get, modify, put,
                                            runParser, switch, (<|>))
import qualified FlatParse.Stateful        as F (Parser, ask)
import qualified Streamly.Data.Fold        as SF (foldl')
import           Streamly.Data.Stream      (Stream, fold, fromList)

import           Control.Monad.Identity    (Identity (..), runIdentity)
import           Control.Monad.Reader      (Reader, runReader)
import qualified Control.Monad.Reader      as R (ask)
import           Data.List                 (foldl')

import           Debug.Trace

type Parser = F.Parser Bool Void IntSet

parseInput :: Parser
parseInput =
  $(switch
      [|case _ of
          "#"  -> addPos
          "."  -> modify (+ 1) >> parseInput
          "\n" -> nextLine|])
    <|> (eof >> pure mempty)

nextLine :: Parser
nextLine = do
  test <- F.ask
  let (w, h) =
        if test
          then (8, 56)
          else (128, 32640)
      newI i = (i + w) .&. h
  modify newI
  parseInput

addPos :: Parser
addPos = get >>= \x -> put (x + 1) >> insert x <$> parseInput

golStencil :: Bool -> Bool -> Stencil Ix2 Int Int
golStencil test isPart2 =
  makeUnsafeStencil (Sz (3 :. 3)) (1 :. 1) (golInside test isPart2)

{-# INLINE golStencil #-}
golInside :: Bool -> Bool -> Ix2 -> (Ix2 -> Int) -> Int
golInside test isPart2 index get
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

gol'' :: Bool -> Bool -> IntSet -> [Int] -> IntSet
gol'' test isPart2 onLights = foldl' (golify' test isPart2 onLights) mempty
  where
    l
      | test = 6
      | otherwise = 100
    bl
      | test = 8
      | otherwise = 128

gol' :: Bool -> Bool -> Stream Identity Int -> IntSet -> IntSet
gol' test isPart2 list onLights =
  runIdentity . fold (SF.foldl' (golify' test isPart2 onLights) mempty) $ list
  where
    l
      | test = 6
      | otherwise = 100
    bl
      | test = 8
      | otherwise = 128

golify' :: Bool -> Bool -> IntSet -> IntSet -> Int -> IntSet
golify' test isPart2 onLights newOns pos
  | length onNeighbours == 3 = insert pos newOns
  | pos `member` onLights && length onNeighbours == 2 = insert pos newOns
  | isPart2
      && (pos .&. (l - 1)) `elem` [0, maxL]
      && shiftR pos bl `elem` [0, maxL] = insert pos newOns
  | otherwise = newOns
  where
    l
      | test = 8
      | otherwise = 128
    bl
      | test = 3
      | otherwise = 7
    maxL
      | test = 5
      | otherwise = 99
    onNeighbours =
      filter (`member` onLights) . map (pos +)
        $ [1, -1, l, -l, l + 1, l - 1, 1 - l, -1 - l]

extract :: Result Void a -> a
extract (OK result _ _) = result
extract _               = error "parser failed"

prettyPrintArray :: Bool -> Array P Ix2 Int -> String
prettyPrintArray test onLights =
  unlines
    [ [prettyPrintDigit (onLights ! x :. y) | x <- [0 .. l - 1]]
    | y <- [0 .. l - 1]
    ]
  where
    l
      | test = 6
      | otherwise = 100
    prettyPrintDigit 1 = '#'
    prettyPrintDigit _ = '.'

prettyPrint :: Bool -> IntSet -> String
prettyPrint test onlines =
  unlines
    [ [prettyPrintBool . member (x + bl * y) $ onlines | x <- [0 .. l - 1]]
    | y <- [0 .. l - 1]
    ]
  where
    l
      | test = 6
      | otherwise = 100
    bl
      | test = 8
      | otherwise = 128
    prettyPrintBool True = '#'
    prettyPrintBool _    = '.'

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
    . A.map
        (\x ->
           if x == '#'
             then 1
             else 0)
    . (fromLists' Seq :: ([String] -> Array P Ix2 Char))
    . map unpack
    . B.lines
  where
    iterations
      | test = 5
      | otherwise = 100
    -- list = fromList [x + bl * y | x <- [0 .. l - 1], y <- [0 .. l - 1]]
    -- l
    --  | test = 6
    --  | otherwise = 100
    -- bl
    --  | test = 8
    --  | otherwise = 128
