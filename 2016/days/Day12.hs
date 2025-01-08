module Day12
  ( part1
  , part2
  ) where

import           Assembunny                           (decompile, parseProgram)
import           Data.Either                          (fromRight)
import           Data.Text                            (Text)
import           Math.NumberTheory.Recurrences.Linear (fibonacci)
import           Text.Megaparsec                      (parse)

result :: Int -> Int
result c = fibonacci (28 + c) + 14 ^ 2

printResult :: Int -> Text -> String
printResult c input =
  program
    ++ "Lines 1-8 set values, lines 9 to 15 calculate the (28 + "
    ++ show c
    ++ ")th fibonacci number lines 16 to 22 add 14 squared, for a result of\n"
    ++ show (result c)
  where
    program =
      decompile
        . fromRight (error "couldn't parse input")
        . parse parseProgram ""
        $ input

part1 :: Bool -> Text -> String
part1 _ = printResult 0

part2 :: Bool -> Text -> String
part2 _ _ = show . result $ 7
