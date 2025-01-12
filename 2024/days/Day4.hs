module Day4
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, assocs, (!?))
import           Data.ByteString            (ByteString)
import           Data.Word                  (Word8)
import           Data.Word8                 (_A, _M, _S, _X)
import           Helpers.Graph              (Pos, dirs, east, north, south,
                                             west)
import           Helpers.Parsers.ByteString (arrayFromByteString)
import           Linear.Vector              ((*^))

type Grid = UArray Pos Word8

compass = northeast : northwest : southeast : southwest : dirs

northeast = north + east

northwest = north + west

southeast = south + east

southwest = south + west

readGrid :: Grid -> Pos -> Maybe [Word8] -> Maybe [Word8]
readGrid grid pos lecture = (:) <$> grid !? pos <*> lecture

findChar :: Word8 -> Grid -> [Pos]
findChar char = map fst . filter ((== char) . snd) . assocs

findXMAS :: Grid -> Pos -> Int
findXMAS grid p =
  length . filter (== Just [_X, _M, _A, _S]) . map readString $ compass
  where
    readString d = foldr (\s -> readGrid grid (p + (s *^ d))) (Just []) [0 .. 3]

findXedMAS :: Grid -> Pos -> Bool
findXedMAS grid p = nesw `elem` mas && nwse `elem` mas
  where
    mas = [Just [_M, _A, _S], Just [_S, _A, _M]]
    nesw = foldr (readGrid grid) (Just []) [p + northeast, p, p + southwest]
    nwse = foldr (readGrid grid) (Just []) [p + northwest, p, p + southeast]

findAll :: Grid -> Int
findAll grid = sum . map (findXMAS grid) . findChar _X $ grid

findAllXed :: Grid -> Int
findAllXed grid = length . filter (findXedMAS grid) . findChar _A $ grid

part1 :: Bool -> ByteString -> String
part1 _ = show . findAll . arrayFromByteString

part2 :: Bool -> ByteString -> String
part2 _ = show . findAllXed . arrayFromByteString
