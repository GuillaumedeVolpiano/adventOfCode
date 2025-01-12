module Day21
  ( part1
  , input
  , part2
  ) where

import           Data.Bifunctor             (bimap)
import           Data.ByteString            (ByteString, unpack)
import           Data.List                  (foldl', minimumBy, nub)
import           Data.MultiSet              (MultiSet, empty, foldOccur,
                                             fromList, insert, insertMany)
import           Data.Ord                   (comparing)
import           Data.Word                  (Word8)
import           Data.Word8                 (_0, _1, _4, _7, _A, _circum,
                                             _greater, _less, _v)
import           Helpers.Parsers.ByteString (digitToInt)
import qualified Helpers.Parsers.ByteString as P (lines)

type Hor = Int

type Ver = Int

data Tree
  = Node (MultiSet Tree)
  | Branch Tree Tree
  | Leaf [Word8]
  deriving (Eq, Ord, Show)

mapMany :: (Ord b) => (a -> b) -> MultiSet a -> MultiSet b
mapMany f = foldOccur (insertMany . f) empty

optify :: ([Word8], [Word8]) -> Tree
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

stringify :: (Hor, Ver) -> [Word8]
stringify (h, v) = horify h . verify v $ [_A]

horify :: Int -> ([Word8] -> [Word8])
horify h
  | h >= 0 = flip (foldr (const (_greater :))) [1 .. h]
  | h <= 0 = flip (foldr (const (_less :))) [1 .. negate h]
  | otherwise = id

verify :: Int -> ([Word8] -> [Word8])
verify v
  | v >= 0 = flip (foldr (const (_circum :))) [1 .. v]
  | v <= 0 = flip (foldr (const (_v :))) [1 .. negate v]
  | otherwise = id

reverseStringify :: (Hor, Ver) -> [Word8]
reverseStringify (h, v) = verify v . horify h $ [_A]

fromCode :: Int -> (Hor, Ver)
fromCode a
  | a == 0 = (1, 0)
  | otherwise = (2 - ((a - 1) `mod` 3), (-1) - ((a - 1) `div` 3))

pressCode :: Int -> (Hor, Ver)
pressCode b
  | b == 0 = (-1, 0)
  | otherwise = (((b - 1) `mod` 3) - 2, ((b - 1) `div` 3) + 1)

shortestPathBot :: Word8 -> Word8 -> (Hor, Ver)
shortestPathBot 65 94  = (-1, 0) -- A ^
shortestPathBot 65 118 = (-1, -1) -- A v
shortestPathBot 65 62  = (0, -1) -- A >
shortestPathBot 65 60  = (-2, -1) -- A <
shortestPathBot 94 60  = (-1, -1) -- ^ <
shortestPathBot 94 62  = (1, -1) -- ^ >
shortestPathBot 118 62 = (1, 0) -- v >
shortestPathBot 118 60 = (-1, 0) -- v <
shortestPathBot a b    = bimap negate negate . shortestPathBot b $ a

botcode :: ([Tree], Word8) -> Word8 -> ([Tree], Word8)
botcode (ks, c) k
  | k == c = (Leaf [_A] : ks, k)
  | c `elem` [_A, _circum] && k == _less =
    ((Leaf . reverseStringify . shortestPathBot c $ k) : ks, k)
  | c == _less && k `elem` [_A, _circum] =
    ((Leaf . stringify . shortestPathBot c $ k) : ks, k)
  | otherwise =
    ( optify
        ( stringify (shortestPathBot c k)
        , reverseStringify (shortestPathBot c k))
        : ks
    , k)

bindTree :: ([Word8] -> Tree) -> Tree -> Tree
bindTree f (Leaf a)     = f a
bindTree f (Node a)     = Node . mapMany (bindTree f) $ a
bindTree f (Branch a b) = Branch (bindTree f a) (bindTree f b)

botOp :: Tree -> Tree
botOp = simplify . bindTree (Node . fromList . fst . foldl' botcode ([], _A))

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

input :: Int -> [Word8] -> Int
input count string = humanOp . (!! count) . iterate botOp $ keyed
  where
    toWatch = [_1, _4, _7]
    keyed = Node . fromList . fst . foldl' keycode ([], _A) $ string
    keycode (ks, c) k
      | c == _A && k `notElem` toWatch =
        ( optify
            ( stringify (pressCode (digitToInt k))
            , reverseStringify (pressCode (digitToInt k)))
            : ks
        , k)
      | c == _A = (Leaf (reverseStringify (pressCode (digitToInt k))) : ks, k)
      | k == _A && c `elem` toWatch =
        (Leaf (stringify (fromCode (digitToInt c))) : ks, k)
      | k == _A =
        ( optify
            ( stringify (fromCode (digitToInt c))
            , reverseStringify (fromCode (digitToInt c)))
            : ks
        , k)
      | k == _0 && c `elem` toWatch =
        ( Leaf (stringify (shortestPathCode (digitToInt c) (digitToInt k))) : ks
        , k)
      | otherwise =
        ( optify
            ( stringify (shortestPathCode (digitToInt c) (digitToInt k))
            , reverseStringify (shortestPathCode (digitToInt c) (digitToInt k)))
            : ks
        , k)

calculate :: Int -> [Word8] -> Int
calculate count string =
  (* input count string) . foldl' (\a b -> b + 10 * a) 0 . map digitToInt . init
    $ string

part1 :: Bool -> ByteString -> String
part1 _ = show . sum . map (calculate 2 . unpack) . P.lines

part2 :: Bool -> ByteString -> String
part2 _ = show . sum . map (calculate 25 . unpack) . P.lines
