module Day9
  ( part1
  , part2
  ) where

import           Data.Bifunctor       (first, second)
import           Data.Char            (digitToInt)
import           Data.Either          (fromRight)
import           Data.List            (sort)
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

type Files = Set FileBlock

type Blocks = Set EmptyBlock

instance Ord FileBlock where
  compare f1 = compare (getPos f1) . getPos

instance Ord EmptyBlock where
  compare e1 = compare (emptyPos e1) . emptyPos

parseInput :: Bool -> Int -> Int -> Parser (Files, Blocks)
parseInput isEmpty depth index =
  parseBlocks isEmpty depth index <|> return (empty, empty)

parseBlocks :: Bool -> Int -> Int -> Parser (Files, Blocks)
parseBlocks isEmpty pos index = do
  blockLength <- digitToInt <$> numberChar
  let pos' = pos + blockLength
      result
        | blockLength == 0 && isEmpty = parseInput False pos index
        | blockLength == 0 = parseInput True pos (index + 1)
        | isEmpty =
          second (insert (EmptyBlock pos blockLength))
            <$> parseInput False pos' index
        | otherwise =
          first (insert (FileBlock index pos blockLength))
            <$> parseInput True pos' (index + 1)
  result

sortDisk :: (Files, Blocks) -> Files
sortDisk (files, blocks)
  | emptyPos emptyBlock > filePos = files
  | otherwise = sortDisk (files', blocks')
  where
    emptyBlock = findMin blocks
    nextFile@(FileBlock index filePos fileLength) = findMax files
    availableSpace = min fileLength . emptyLength $ emptyBlock
    fileBlock = FileBlock index (emptyPos emptyBlock) availableSpace
    nextFile' = FileBlock index filePos (fileLength - availableSpace)
    emptyBlock' =
      EmptyBlock
        (emptyPos emptyBlock + availableSpace)
        (emptyLength emptyBlock - availableSpace)
    files'
      | availableSpace == fileLength =
        insert fileBlock . delete nextFile $ files
      | otherwise =
        insert fileBlock . insert nextFile' . delete nextFile $ files
    blocks'
      | availableSpace == emptyLength emptyBlock = delete emptyBlock blocks
      | otherwise = insert emptyBlock' . delete emptyBlock $ blocks

defragment :: (Files, Blocks) -> Files
defragment (files, blocks) =
  packFiles files blocks [lastIndex,lastIndex - 1 .. 0]
  where
    lastIndex = getIndex . findMax $ files

packFiles :: Files -> Blocks -> [Int] -> Files
packFiles files blocks [] = files
packFiles files blocks (i:is)
  | S.null availableEmptyBlocks = packFiles files blocks is
  | otherwise = packFiles files' blocks' is
  where
    file = findMin . S.filter ((== i) . getIndex) $ files
    availableEmptyBlocks =
      S.filter ((>= getLength file) . emptyLength)
        . S.filter ((<= getPos file) . emptyPos)
        $ blocks
    block = findMin availableEmptyBlocks
    file' = file {getPos = emptyPos block}
    files' = insert file' . delete file $ files
    block' =
      EmptyBlock
        (emptyPos block + getLength file)
        (emptyLength block - getLength file)
    blocks'
      | emptyLength block == getLength file = delete block blocks
      | otherwise = insert block' . delete block $ blocks

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
    . fromRight (empty, empty)
    . parse (parseInput False 0 0) ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . foldr ((+) . checksum) 0
    . defragment
    . fromRight (empty, empty)
    . parse (parseInput False 0 0) ""
