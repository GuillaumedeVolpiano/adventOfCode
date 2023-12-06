import           Data.List.Split    (splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

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

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let emptyFSZipper = (Folder "/" [], [])
      (fileSystem, _) = goToRoot . parseTree (lines input) $ emptyFSZipper
      sizes = sizeList fileSystem []
      available = 70000000 - maximum sizes
  putStrLn "part 1"
  print . sum . filter (<= 100000) $ sizes
  putStrLn "part 2"
  print . minimum . filter (>= (30000000 - available)) $ sizes
