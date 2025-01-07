module Day22
  ( part1
  , part2
  , getNSecrets
  , getNthSecret
  , getDiffs
  ) where

import           Control.Monad              (forM_)
import           Control.Monad.ST           (ST, runST)
import           Data.Bits                  (shiftL, shiftR, xor, (.&.))
import           Data.Either                (fromRight)
import           Data.Massiv.Array          (Comp (..), Ix4, P, Sz (Sz4),
                                             maximum', toIx4)
import           Data.Massiv.Array.Mutable  (MArray, modify_, newMArray, write_)
import qualified Data.Massiv.Array.Mutable  as MA (read)
import           Data.Massiv.Array.Unsafe   (unsafeFreeze)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, manyTill, parse)
import           Text.Megaparsec.Char       (eol)
import           Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Parser [Int]
parseInput = manyTill parseNumber eof

parseNumber :: Parser Int
parseNumber = do
  num <- decimal
  eol
  return num

-- used in tests
getDiffs :: [Int] -> [Int]
getDiffs a = zipWith (-) (tail a) a

sequences :: [Int] -> ST s Int
sequences salts = do
  bananas <- newMArray (Sz4 20 20 20 20) 0 :: ST s (MArray s P Ix4 Int)
  seen <- newMArray (Sz4 20 20 20 20) 0 :: ST s (MArray s P Ix4 Int)
  forM_ salts $ \salt -> do
    let initIndex = initSequence salt
    innerSeq 4 bananas seen salt initIndex
  maximum' <$> unsafeFreeze Seq bananas

innerSeq ::
     Int
  -> MArray s P Ix4 Int
  -> MArray s P Ix4 Int
  -> Int
  -> (Int, Int, Int, Int, Int)
  -> ST s ()
innerSeq 2001 _ _ _ _ = return ()
innerSeq counter bananas seen salt (a, b, c, d, e) = do
  let f = secret e
      e' = e `mod` 10
      d' = 9 + d - e'
      counter' = counter + 1
      nexts = (b, c, d', e', f)
      index = toIx4 (a, b, c, d')
  curVal <- MA.read bananas index
  redundant <- MA.read seen index
  if redundant == Just salt
    then innerSeq counter' bananas seen salt nexts
    else write_ seen index salt
           >> write_ bananas index (e' + fromJust curVal)
           >> innerSeq counter' bananas seen salt nexts

initSequence :: Int -> (Int, Int, Int, Int, Int)
initSequence salt = (9 - b' + a', 9 - c' + b', 9 - d' + c', d', e)
  where
    b = secret salt
    c = secret b
    d = secret c
    e = secret d
    a' = salt `mod` 10
    b' = b `mod` 10
    c' = c `mod` 10
    d' = d `mod` 10

secret :: Int -> Int
secret a = prune sec3
  where
    sec1 = mix a . shiftL a $ 6
    sec2 = mix sec1 . flip shiftR 5 . prune $ sec1
    sec3 = mix sec2 . flip shiftL 11 . prune $ sec2

mix :: Int -> Int -> Int
mix a b = a `xor` b

prune :: Int -> Int
prune a = a .&. 16777215 -- 16777216 - 1

getNSecrets :: Int -> Int -> [Int]
getNSecrets n = take n . iterate secret

getNthSecret :: Int -> Int -> Int
getNthSecret n = (!! n) . iterate secret

bestBananas :: [Int] -> Int
bestBananas salts = runST $ sequences salts

part1 :: Bool -> Text -> String
part1 _ =
  show
    . sum
    . map (getNthSecret 2000)
    . fromRight (error "parse failed")
    . parse parseInput "day22"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . bestBananas
    . fromRight (error "parse failed")
    . parse parseInput "day22"
