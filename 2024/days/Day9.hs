module Day9
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (first, second)
import           Data.Char            (digitToInt)
import           Data.Either          (fromRight)
import           Data.List            as L (null, sort)
import           Data.Set             as S (Set, delete, empty, filter, findMax,
                                            findMin, insert, null)
import           Data.Text            (Text)
import           Helpers.Parsers.Text (Parser)
import           Text.Megaparsec      (parse, (<|>))
import           Text.Megaparsec.Char (eol, numberChar)

data FileBlock = FileBlock
  { getIndex  :: Index
  , getPos    :: Pos
  , getLength :: Length
  } deriving (Show, Eq)

data EmptyBlock = EmptyBlock
  { emptyPos    :: Pos
  , emptyLength :: Length
  } deriving (Show, Eq)

type Index = Int

type Pos = Int

type Length = Int

type Files = [FileBlock]

type Blocks = Set EmptyBlock

instance Ord FileBlock where
  compare f1 = compare (getPos f1) . getPos

instance Ord EmptyBlock where
  compare e1 = compare (emptyPos e1) . emptyPos

parseInput :: Bool -> Int -> Int -> Files -> Parser (Files, Blocks)
parseInput isEmpty depth index files =
  parseBlocks isEmpty depth index files <|> return (files, empty)

parseBlocks :: Bool -> Int -> Int -> Files -> Parser (Files, Blocks)
parseBlocks isEmpty pos index files = do
  blockLength <- digitToInt <$> numberChar
  let pos' = pos + blockLength
      result
        | blockLength == 0 && isEmpty = parseInput False pos index files
        | blockLength == 0 = parseInput True pos (index + 1) files
        | isEmpty =
          second (insert (EmptyBlock pos blockLength))
            <$> parseInput False pos' index files
        | otherwise =
          parseInput
            True
            pos'
            (index + 1)
            (FileBlock index pos blockLength : files)
  result

sortDisk :: (Files, Blocks) -> Files
sortDisk (nextFile@(FileBlock index filePos fileLength):files, blocks)
  | emptyPos emptyBlock > filePos = []
  | otherwise = fileBlock : sortDisk (files', blocks')
  where
    emptyBlock = findMin blocks
    availableSpace = min fileLength . emptyLength $ emptyBlock
    fileBlock = FileBlock index (emptyPos emptyBlock) availableSpace
    nextFile' = FileBlock index filePos (fileLength - availableSpace)
    emptyBlock' =
      EmptyBlock
        (emptyPos emptyBlock + availableSpace)
        (emptyLength emptyBlock - availableSpace)
    files'
      | availableSpace == fileLength = files
      | otherwise = nextFile' : files
    blocks'
      | availableSpace == emptyLength emptyBlock = delete emptyBlock blocks
      | otherwise = insert emptyBlock' . delete emptyBlock $ blocks

defragment :: (Files, Blocks) -> Files
defragment (file:files, blocks)
  | L.null files = []
  | S.null availableEmptyBlocks = file : defragment (files, leftBlocks)
  | otherwise = file' : defragment (files, blocks')
  where
    leftBlocks = S.filter ((<= getPos file) . emptyPos) blocks
    availableEmptyBlocks =
      S.filter ((>= getLength file) . emptyLength) leftBlocks
    block = findMin availableEmptyBlocks
    file' = file {getPos = emptyPos block}
    block' =
      EmptyBlock
        (emptyPos block + getLength file)
        (emptyLength block - getLength file)
    blocks'
      | emptyLength block == getLength file = delete block leftBlocks
      | otherwise = insert block' . delete block $ leftBlocks
    --

-- The sum of n numbers from filePos to filePos + fileLength - 1 is fileLength *
-- filePos + ((fileLength * (fileLength - 1)) / 2)
checksum :: FileBlock -> Int
checksum (FileBlock index filePos fileLength) =
  (fileLength * filePos + div (fileLength * (fileLength - 1)) 2) * index

part1 :: Bool -> Text -> String
part1 _ =
  show
    . foldr ((+) . checksum) 0
    . sortDisk
    . fromRight ([], empty)
    . parse (parseInput False 0 0 []) ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . foldr ((+) . checksum) 0
    . defragment
    . fromRight ([], empty)
    . parse (parseInput False 0 0 []) ""
