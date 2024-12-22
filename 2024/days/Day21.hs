module Day21
  ( part1
  , input
  , part2
  ) where

import           Data.Bifunctor (bimap)
import           Data.Char      (digitToInt)
import           Data.List      (foldl', minimumBy, nub)
import           Data.MultiSet  (MultiSet, empty, foldOccur, fromList, insert,
                                 insertMany)
import           Data.Ord       (comparing)
import           Data.Text      (Text, unpack)
import qualified Data.Text      as T (lines)

type Hor = Int

type Ver = Int

data Tree
  = Node (MultiSet Tree)
  | Branch Tree Tree
  | Leaf String
  deriving (Eq, Ord, Show)

mapMany :: (Ord b) => (a -> b) -> MultiSet a -> MultiSet b
mapMany f = foldOccur (insertMany . f) empty

optify :: (String, String) -> Tree
optify (a, b)
  | a == b = Leaf a
  | otherwise = Branch (Leaf a) (Leaf b)

shortestPathCode :: Int -> Int -> (Hor, Ver)
shortestPathCode a b
  | a > 0 && b > 0 =
    ( ((b - 1) `mod` 3) - ((a - 1) `mod` 3)
    , ((b - 1) `div` 3) - ((a - 1) `div` 3))
  | a == 0 = (((b - 1) `mod` 3) - 1, ((b - 1) `div` 3) + 1)
  | b == 0 = (1 - ((a - 1) `mod` 3), (-1) - ((a - 1) `div` 3))

stringify :: (Hor, Ver) -> String
stringify (h, v) = horify h . verify v $ "A"

horify :: Int -> (String -> String)
horify h
  | h >= 0 = flip (foldr (const ('>' :))) [1 .. h]
  | h <= 0 = flip (foldr (const ('<' :))) [1 .. negate h]
  | otherwise = id

verify :: Int -> (String -> String)
verify v
  | v >= 0 = flip (foldr (const ('^' :))) [1 .. v]
  | v <= 0 = flip (foldr (const ('v' :))) [1 .. negate v]
  | otherwise = id

reverseStringify :: (Hor, Ver) -> String
reverseStringify (h, v) = verify v . horify h $ "A"

fromCode :: Int -> (Hor, Ver)
fromCode a
  | a == 0 = (1, 0)
  | otherwise = (2 - ((a - 1) `mod` 3), (-1) - ((a - 1) `div` 3))

pressCode :: Int -> (Hor, Ver)
pressCode b
  | b == 0 = (-1, 0)
  | otherwise = (((b - 1) `mod` 3) - 2, ((b - 1) `div` 3) + 1)

shortestPathBot :: Char -> Char -> (Hor, Ver)
shortestPathBot 'A' '^' = (-1, 0)
shortestPathBot 'A' 'v' = (-1, -1)
shortestPathBot 'A' '>' = (0, -1)
shortestPathBot 'A' '<' = (-2, -1)
shortestPathBot '^' '<' = (-1, -1)
shortestPathBot '^' '>' = (1, -1)
shortestPathBot 'v' '>' = (1, 0)
shortestPathBot 'v' '<' = (-1, 0)
shortestPathBot a b     = bimap negate negate . shortestPathBot b $ a

botcode :: ([Tree], Char) -> Char -> ([Tree], Char)
botcode (ks, c) k
  | k == c = (Leaf "A" : ks, k)
  | c `elem` "A^" && k == '<' =
    ((Leaf . reverseStringify . shortestPathBot c $ k) : ks, k)
  | c == '<' && k `elem` "A^" =
    ((Leaf . stringify . shortestPathBot c $ k) : ks, k)
  | otherwise =
    ( optify
        ( stringify (shortestPathBot c k)
        , reverseStringify (shortestPathBot c k))
        : ks
    , k)

bindTree :: (String -> Tree) -> Tree -> Tree
bindTree f (Leaf a)     = f a
bindTree f (Node a)     = Node . mapMany (bindTree f) $ a
bindTree f (Branch a b) = Branch (bindTree f a) (bindTree f b)

botOp :: Tree -> Tree
botOp = simplify . bindTree (Node . fromList . fst . foldl' botcode ([], 'A'))

simplify :: Tree -> Tree
simplify (Leaf a) = Leaf a
simplify n@(Node xs)
  | any isNode xs = Node xs'
  | otherwise = n
  where
    xs' = foldOccur flatten empty xs
simplify (Branch a b)
  | humanOp a' < humanOp b' = a'
  | humanOp b' < humanOp a = b'
  | otherwise = Branch a' b'
  where
    a' = simplify a
    b' = simplify b

isNode :: Tree -> Bool
isNode (Node _) = True
isNode _        = False

flatten :: Tree -> Int -> MultiSet Tree -> MultiSet Tree
flatten (Leaf a) b s = insertMany (Leaf a) b s
flatten o@(Branch _ _) a s = insertMany (simplify o) a s
flatten (Node xs) a s =
  foldOccur (\item count -> insertMany (simplify item) (a * count)) s xs

humanOp :: Tree -> Int
humanOp (Leaf a)     = length a
humanOp (Node xs)    = sum . mapMany humanOp $ xs
humanOp (Branch a b) = min (humanOp a) (humanOp b)

input :: Int -> String -> Int
input count string = humanOp . (!! count) . iterate botOp $ keyed
  where
    keyed = Node . fromList . fst . foldl' keycode ([], 'A') $ string
    keycode (ks, c) k
      | c == 'A' && k `notElem` "147" =
        ( optify
            ( stringify (pressCode (digitToInt k))
            , reverseStringify (pressCode (digitToInt k)))
            : ks
        , k)
      | c == 'A' = (Leaf (reverseStringify (pressCode (digitToInt k))) : ks, k)
      | k == 'A' && c `elem` "147" =
        (Leaf (stringify (fromCode (digitToInt c))) : ks, k)
      | k == 'A' =
        ( optify
            ( stringify (fromCode (digitToInt c))
            , reverseStringify (fromCode (digitToInt c)))
            : ks
        , k)
      | k == '0' && c `elem` "147" =
        ( Leaf (stringify (shortestPathCode (digitToInt c) (digitToInt k))) : ks
        , k)
      | otherwise =
        ( optify
            ( stringify (shortestPathCode (digitToInt c) (digitToInt k))
            , reverseStringify (shortestPathCode (digitToInt c) (digitToInt k)))
            : ks
        , k)

calculate :: Int -> String -> Int
calculate count string = (* input count string) . read . init $ string

part1 :: Bool -> Text -> String
part1 _ = show . sum . map (calculate 2 . unpack) . T.lines

part2 :: Bool -> Text -> String
part2 _ = show . sum . map (calculate 25 . unpack) . T.lines
