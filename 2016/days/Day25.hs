module Day25
  ( part1
  , part2
  ) where

import           Assembunny      (decompile, parseProgram)
import           Data.Char       (digitToInt)
import           Data.Either     (fromRight)
import           Data.Text       (Text)
import           Text.Megaparsec (parse)

printResult :: Text -> String
printResult input =
  program
    ++ "This outputs indefinitely the binary representation of 2572 + a. So we need the smallest number a such that 2572 + a is of the form 101010…. The binary representation of 2572 is 101000001100, so the number we are looking for is 10011110, that is\n"
    ++ show (foldr (\a b -> digitToInt a + 2 * b) 0 "01111001")
  where
    program =
      decompile . fromRight (error "parser failed") . parse parseProgram "day25"
        $ input
    binaryRep = reverse . binaryRepRev
    binaryRepRev 1 = "1"
    binaryRepRev i
      | even i = '0' : binaryRep (i `div` 2)
      | otherwise = '1' : binaryRep (i `div` 2)

part1 :: Bool -> Text -> String
part1 _ = printResult

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
