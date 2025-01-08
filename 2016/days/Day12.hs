module Day12
  ( part1
  , part2
  ) where

import           Assembunny      (decompile, execute, parseProgram, setRegister)
import           Data.Either     (fromRight)
import           Data.Text       (Text)
import           Text.Megaparsec (parse)

part1 :: Bool -> Text -> String
part1 _ =
  show
    . execute
    . fromRight (error "couldn't parse input")
    . parse parseProgram ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . decompile
    -- . execute
    -- . (\p -> setRegister p 'c' 1)
    . fromRight (error "couldn't parse input")
    . parse parseProgram ""
