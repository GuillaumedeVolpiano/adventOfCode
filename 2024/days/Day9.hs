module Day9
  ( part1
  , part2
  ) where

import           Control.Monad.State  (State, evalState, get, put)
import           Data.Bifunctor       (first, second)
import           Data.Char            (digitToInt)
import           Data.Either          (fromRight)
import           Data.Function        (on)
import           Data.IntMap          as M (IntMap, delete, fromList, insert,
                                            lookup, notMember, null, size, (!))
import           Data.IntSet          as S (IntSet, delete, empty, findMin,
                                            insert, null, singleton)
import           Data.List            as L (delete, groupBy, minimumBy, null,
                                            partition, sort)
import           Data.Maybe           (fromJust, isJust)
import           Data.Ord             (comparing)
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Text.Megaparsec      (ParsecT, runParserT, (<|>))
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

type Blocks = [EmptyBlock]

instance Ord EmptyBlock where
  compare e1 e2 =
    compare (emptyLength e1) (emptyLength e2)
      `mappend` compare (emptyPos e1) (emptyPos e2)

type BlockMap = IntMap IntSet

type Parser = ParsecT Void Text (State (Bool, Int, Int, Files))

parseInput :: Parser (Files, Blocks)
parseInput =
  parseBlocks
    <|> (do
           (_, _, _, files) <- get
           return (files, []))

parseBlocks :: Parser (Files, Blocks)
parseBlocks = do
  (isEmpty, pos, index, files) <- get
  blockLength <- digitToInt <$> numberChar
  let pos' = pos + blockLength
      state
        | blockLength == 0 && isEmpty = put (False, pos, index, files)
        | blockLength == 0 = put (True, pos, index + 1, files)
        | isEmpty = put (False, pos', index, files)
        | otherwise =
          put (True, pos', index + 1, FileBlock index pos blockLength : files)
  state
  if isEmpty
    then second (EmptyBlock pos blockLength :) <$> parseInput
    else parseInput

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

buildBlockMap :: Blocks -> BlockMap
buildBlockMap =
  M.fromList
    . map
        (foldr
           (\(EmptyBlock pos el) (_, s) -> (el, S.insert pos s))
           (0, S.empty))
    . groupBy ((==) `on` emptyLength)
    . sort

defragment :: (Files, BlockMap) -> Files
defragment (file:files, blocks)
  | L.null files = []
  | M.null blocks = file : files
  | L.null left = file : defragment (files, blocks')
  | otherwise = file' : defragment (files, blocks''')
  where
    availableEmptyBlocks =
      map (second fromJust)
        . filter (isJust . snd)
        . map (\l -> (l, S.findMin <$> M.lookup l blocks))
        $ [getLength file .. 9]
    (left, right) = partition ((< getPos file) . snd) availableEmptyBlocks
    (el, pos) = minimumBy (comparing snd) left
    file' = file {getPos = pos}
    el' = el - getLength file
    pos' = pos + getLength file
    oldBlocks = S.delete pos . (!) blocks $ el
    newBlocks
      | el' `notMember` blocks' = singleton pos'
      | otherwise = S.insert pos' . (!) blocks' $ el'
    blocks' = foldr (M.delete . fst) blocks right
    blocks''
      | el' == 0 = blocks'
      | otherwise = M.insert el' newBlocks blocks'
    blocks'''
      | S.null oldBlocks = M.delete el blocks''
      | otherwise = M.insert el oldBlocks blocks''

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
    . flip evalState (False, 0, 0, [])
    . runParserT parseInput ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . foldr ((+) . checksum) 0
    . defragment
    . second buildBlockMap
    . fromRight ([], [])
    . flip evalState (False, 0, 0, [])
    . runParserT parseInput ""
