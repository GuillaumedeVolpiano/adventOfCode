module Day9
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (first, second)
import           Data.Char            (digitToInt)
import           Data.Either          (fromRight)
import           Data.List            (delete, sort)
import           Data.Text            (Text)
import           Helpers.Parsers.Text (Parser)
import           Text.Megaparsec      (parse, (<|>))
import           Text.Megaparsec.Char (eol, numberChar)

import           Debug.Trace

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

type Blocks = [EmptyBlock]

instance Ord EmptyBlock where
  compare e1 e2 = compare (emptyPos e1) (emptyPos e2)

parseInput :: Bool -> Int -> Int -> Files -> Parser (Files, Blocks)
parseInput isEmpty depth index files =
  parseBlocks isEmpty depth index files <|> return (files, [])

parseBlocks :: Bool -> Int -> Int -> Files -> Parser (Files, Blocks)
parseBlocks isEmpty pos index files = do
  blockLength <- digitToInt <$> numberChar
  let pos' = pos + blockLength
      result
        | blockLength == 0 && isEmpty = parseInput False pos index files
        | blockLength == 0 = parseInput True pos (index + 1) files
        | isEmpty =
          second (EmptyBlock pos blockLength :)
            <$> parseInput False pos' index files
        | otherwise =
          parseInput
            True
            pos'
            (index + 1)
            (FileBlock index pos blockLength : files)
  result

sortDisk :: (Files, Blocks) -> Files
sortDisk (nextFile@(FileBlock index filePos fileLength):files, emptyBlock:blocks)
  | emptyPos emptyBlock > filePos = nextFile : files
  | otherwise = fileBlock : sortDisk (files', blocks')
  where
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
      | availableSpace == emptyLength emptyBlock = blocks
      | otherwise = emptyBlock' : blocks

defragment :: (Files, Blocks) -> Files
defragment (file:files, blocks)
  | null files = []
  | null availableEmptyBlocks = file : defragment (files, leftBlocks)
  | otherwise = file' : defragment (files, blocks')
  where
    leftBlocks = takeWhile ((< getPos file) . emptyPos) blocks
    availableEmptyBlocks = filter ((>= getLength file) . emptyLength) leftBlocks
    block = head availableEmptyBlocks
    file' = file {getPos = emptyPos block}
    block' =
      EmptyBlock
        (emptyPos block + getLength file)
        (emptyLength block - getLength file)
    blocks'
      | emptyLength block == getLength file = delete block leftBlocks
      | otherwise = block' : delete block leftBlocks
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
    . fromRight ([], [])
    . parse (parseInput False 0 0 []) ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . foldr ((+) . checksum) 0
    . defragment
    . second sort
    . fromRight ([], [])
    . parse (parseInput False 0 0 []) ""
