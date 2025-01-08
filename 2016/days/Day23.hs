module Day23
  ( part1
  , part2
  ) where

import           Assembunny                           (decompile, parseProgram)
import           Data.Either                          (fromRight)
import qualified Data.List.Infinite                   as IL ((!!))
import           Data.Text                            (Text)
import           Math.NumberTheory.Recurrences.Linear (factorial)
import           Text.Megaparsec                      (parse)

result :: Int -> Int
result val = (79 * 77) + factorial IL.!! fromIntegral val

printResult :: Int -> Text -> String
printResult val input =
  program
    ++ " lines 1-18 calculate the factorial of the input, while adjusting lines 18-onwards to calculate 79 * 77 and add it to the first result, giving a total result of\n"
    ++ show (result val)
  where
    program =
      decompile . fromRight (error "parser failed") . parse parseProgram "day23"
        $ input

part1 :: Bool -> Text -> String
part1 _ = printResult 7

part2 :: Bool -> Text -> String
part2 _ _ = show . result $ 12
