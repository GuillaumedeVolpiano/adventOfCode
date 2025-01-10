module Day18
  ( part1
  , part2
  ) where

import           Data.Bifunctor        (bimap)
import           Data.Bits             (popCount, shiftL, shiftR, xor, (.&.),
                                        (.|.))
import           Data.Either           (fromRight)
import           Data.List             (unfoldr)
import           Data.Text             (Text)
import           Data.WideWord.Word128 (Word128)
import           Helpers.Parsers.Text  (Parser)
import           Text.Megaparsec       (parse, (<|>))
import           Text.Megaparsec.Char  (char, eol)

parseInput :: Parser (Word128, Word128)
parseInput =
  (char '^'
     >> bimap ((.|. 1) . flip shiftL 1) ((.|. 1) . flip shiftL 1) <$> parseInput)
    <|> (char '.' >> bimap (`shiftL` 1) ((.|. 1) . flip shiftL 1) <$> parseInput)
    <|> (eol >> return (0, 0))

expand :: (Word128, Word128) -> Maybe (Int, (Word128, Word128))
expand (traps, mask) =
  Just (popCount (traps `xor` mask), (traps' .&. mask, mask))
  where
    traps' = shiftR traps 1 `xor` shiftL traps 1

countSafe :: Int -> Text -> Int
countSafe height =
  sum
    . take height
    . unfoldr expand
    . fromRight (error "parser failed")
    . parse parseInput "day18"

part1 :: Bool -> Text -> String
part1 test = show . countSafe n
  where
    n
      | test = 10
      | otherwise = 40

part2 :: Bool -> Text -> String
part2 _ = show . countSafe 400000
