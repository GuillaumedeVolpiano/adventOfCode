module Day12
  ( part1
  , part2
  , validGrids
  ) where

import           Data.Word                (Word8)
import           Data.Word8               (_colon, _lf, _space, _x)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (drain, foldl', sum, toList)
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (eof, many, manyTill, one,
                                                satisfy)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)

data Grid = G Int Int Int

parseGrid :: Parser Word8 IO Grid
parseGrid = do
  w <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_x)) toNumber
  h <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_colon)) toNumber
  s <- P.manyTill (P.satisfy (==_space) >> P.many (P.satisfy isDigit) toNumber) (P.satisfy (==_lf)) F.sum
  pure $ G w h s

toNumber :: Fold IO Word8 Int
toNumber = flip F.foldl' 0 $ \acc w -> acc * 10 + digit w

parseGrids :: Parser Word8 IO [Grid]
parseGrids = P.manyTill parseGrid P.eof F.toList

parseShapes :: Parser Word8 IO ()
parseShapes = P.many
  (P.manyTill P.one (P.satisfy (==_lf) >> P.satisfy (==_lf)) F.drain)
  F.drain

parseInput :: Parser Word8 IO [Grid]
parseInput = parseShapes >> parseGrids

validGrids :: Stream IO Word8 -> IO Int
validGrids s = do
  grids <- either (error "Parser error") id <$> S.parse parseInput s
  pure . length $ filter (\(G w h st) -> w * h >= 9 * st) grids

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = validGrids s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print "Part 2"
