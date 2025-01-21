{-# LANGUAGE TemplateHaskell #-}

module Day6
  ( part1
  , part2
  ) where

import           Control.Applicative         (some, (<|>))
import           Control.Monad               (forM_)
import           Control.Monad.ST            (ST, runST)
import           Data.Bits                   (shiftL)
import           Data.ByteString             (ByteString)
import           Data.ByteString.UTF8        (fromString)
import           Data.Vector.Unboxed         (unsafeFreeze)
import qualified Data.Vector.Unboxed         as V (filter, length)
import           Data.Vector.Unboxed.Mutable (MVector, unsafeModify, unsafeRead,
                                              unsafeWrite)
import qualified Data.Vector.Unboxed.Mutable as MV (foldr', replicate)
import           Data.Void                   (Void)
import           FlatParse.Basic             (anyAsciiDecimalInt, char,
                                              runParser, string, switch)
import qualified FlatParse.Basic             as F (char, string)
import           Helpers.Parsers.FlatParse   (Parser, extract)

data Modify
  = TurnOn [Int]
  | TurnOff [Int]
  | Toggle [Int]

posify :: Int -> Int -> Int
posify x y = x + shiftL y 10

tokenise :: Parser ([Int] -> Modify)
tokenise =
  $(switch
      [|case _ of
          "turn on "  -> pure TurnOn
          "turn off " -> pure TurnOff
          "toggle "   -> pure Toggle|])

parseInput :: Parser [Modify]
parseInput = some parseLine

parseLine :: Parser Modify
parseLine = do
  op <- tokenise
  lx <- anyAsciiDecimalInt
  $(F.char ',')
  ly <- anyAsciiDecimalInt
  $(F.string " through ")
  ux <- anyAsciiDecimalInt
  $(F.char ',')
  uy <- anyAsciiDecimalInt
  $(F.char '\n')
  pure . op $ [posify x y | x <- [lx .. ux], y <- [ly .. uy]]

solvePart1 :: [Modify] -> ST s Int
solvePart1 mods = do
  lights <- MV.replicate (2 ^ 20) False
  forM_ mods $ \mod -> do
    operate1 mod lights
  V.length . V.filter id <$> unsafeFreeze lights

solvePart2 :: [Modify] -> ST s Int
solvePart2 mods = do
  lights <- MV.replicate (2 ^ 20) 0
  forM_ mods $ \mod -> do
    operate2 mod lights
  MV.foldr' (+) 0 lights

operate1 :: Modify -> MVector s Bool -> ST s ()
operate1 (TurnOn poss) lights = do
  forM_ poss $ \pos -> do
    unsafeWrite lights pos True
operate1 (TurnOff poss) lights = do
  forM_ poss $ \pos -> do
    unsafeWrite lights pos False
operate1 (Toggle poss) lights = do
  forM_ poss $ \pos -> do
    onOff <- unsafeRead lights pos
    unsafeWrite lights pos $ not onOff

operate2 :: Modify -> MVector s Int -> ST s ()
operate2 (TurnOn poss) lights = do
  forM_ poss $ \pos -> do
    unsafeModify lights (+ 1) pos
operate2 (TurnOff poss) lights = do
  forM_ poss $ \pos -> do
    unsafeModify
      lights
      (\x ->
         if x > 1
           then x - 1
           else 0)
      pos
operate2 (Toggle poss) lights = do
  forM_ poss $ \pos -> do
    unsafeModify lights (+ 2) pos

part1 :: Bool -> ByteString -> String
part1 _ input = show solved
  where
    solved = runST $ solvePart1 mods
    mods = extract . runParser parseInput $ input :: [Modify]

part2 :: Bool -> ByteString -> String
part2 _ input = show solved
  where
    solved = runST $ solvePart2 mods
    mods = extract . runParser parseInput $ input
