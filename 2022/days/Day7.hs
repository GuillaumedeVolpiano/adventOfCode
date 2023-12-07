module Day7 (part1, part2) where
import           Data.List.Split    (splitOn)

day = 7

type Name = String

type Size = Int

data FSItem
  = File Size Name
  | Folder Name [FSItem]
  deriving (Show)

data FSPos =
  FSPos Name [FSItem] [FSItem]
  deriving (Show)

type FSZipper = (FSItem, [FSPos])

emptyFSZipper = (Folder "/" [], [])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSPos name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

goToRoot :: FSZipper -> FSZipper
goToRoot root@(Folder "/" cont, pos) = root
goToRoot other                       = goToRoot . fsUp $ other

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) = (item, FSPos folderName ls rs : bs)
  where
    (ls, item:rs) = break (nameIs name) items

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File _ fileName)     = name == fileName

parseTree :: [String] -> FSZipper -> FSZipper
parseTree [] zipper = zipper
parseTree (x:xs) zipper
  | x == "$ cd /" = parseTree xs $ goToRoot zipper
  | x == "$ cd .." = parseTree xs $ fsUp zipper
  | take 4 x == "$ cd" = parseTree xs $ fsTo (drop 5 x) zipper
  | x == "$ ls" = parseTree cont $ populate rem zipper
  where
    (rem, cont) = break isInst xs
    isInst (l:ls) = l == '$'

populate :: [String] -> FSZipper -> FSZipper
populate [] zipper = zipper
populate (x:xs) (Folder foldername items, bs)
  | firstWord == "dir" =
    populate xs (Folder foldername (Folder secondWord [] : items), bs)
  | otherwise =
    populate
      xs
      (Folder foldername (File (read firstWord) secondWord : items), bs)
  where
    (firstWord:secondWord:_) = words x

folderSize :: FSItem -> Int
folderSize (Folder _ items) = sum . map size $ items
  where
    size folder@(Folder _ _) = folderSize folder
    size (File size _)       = size

sizeList :: FSItem -> [Int] -> [Int]
sizeList folder@(Folder name items) list =
  folderSize folder : populate items list
  where
    populate [] aList              = aList
    populate ((File _ _):xs) aList = populate xs aList
    populate (x:xs) aList          = populate xs $ sizeList x aList

part1 :: Bool -> String -> String
part1 _ input = show . sum . filter (<= 10000) $ sizes
  where
    sizes = sizeList fileSystem []
    (fileSystem, _) = goToRoot . parseTree (lines input) $ emptyFSZipper

part2 :: Bool -> String -> String
part2 _ input = show . minimum . filter (>= (30000000 - available)) $ sizes
  where
    sizes = sizeList fileSystem []
    (fileSystem, _) = goToRoot . parseTree (lines input) $ emptyFSZipper
    available = 70000000 - maximum sizes
