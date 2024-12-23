module Day23
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Data.List            (intercalate, maximumBy, sortBy)
import           Data.Map.Strict      (Map, assocs, insertWith, keys, (!))
import qualified Data.Map.Strict      as M (empty)
import           Data.Ord             (comparing)
import           Data.Set             (Set, delete, difference, fromList,
                                       insert, intersection, member, singleton,
                                       size, toList, union, unions)
import qualified Data.Set             as S (empty, filter, map, null)
import           Data.Text            (Text, pack, unpack)
import qualified Data.Text            as T (head)
import           Helpers.Parsers.Text (Parser)
import           Text.Megaparsec      (eof, manyTill, parse, (<|>))
import           Text.Megaparsec.Char (char, eol, lowerChar)

type LAN = Map Node (Set Node)

type Node = Text

newtype SizeSort = SizeSort
  { getSet :: Set Node
  } deriving (Show, Eq)

instance Ord SizeSort where
  compare a b =
    compare (length . getSet $ a) (length . getSet $ b) `mappend` compare a b

parseInput :: Parser LAN
parseInput = parseEdge <|> (eof >> return M.empty)

parseEdge :: Parser LAN
parseEdge = do
  a <- pack <$> manyTill lowerChar (char '-')
  b <- pack <$> manyTill lowerChar eol
  insertWith union a (singleton b) . insertWith union b (singleton a)
    <$> parseInput

findTriconnectedTs :: LAN -> Int
findTriconnectedTs lan =
  size . unions . map (triplets lan) . filter ((== 't') . T.head) . keys $ lan

triplets :: LAN -> Node -> Set (Set Node)
triplets lan node = unions . S.map thirds $ neighbours
  where
    neighbours = lan ! node
    thirds x =
      S.map (fromList . (: [node, x])) . S.filter (member node . (lan !))
        $ lan ! x

bronKerboschOrdering ::
     LAN -> [Node] -> Set Node -> Set Node -> Set Node -> Set Node
bronKerboschOrdering lan ordering nodes seen clique
  | null ordering = clique
  | otherwise = bronKerboschOrdering lan ordering' nodes' seen' clique''
  where
    (v:ordering') = ordering
    nodes' = delete v nodes
    seen' = insert v seen
    clique''
      | size clique' > size clique = clique'
      | otherwise = clique
    clique' =
      bronKerboschPivot
        lan
        (singleton v)
        (intersection nodes $ lan ! v)
        (intersection seen $ lan ! v)

bronKerboschPivot :: LAN -> Set Node -> Set Node -> Set Node -> Set Node
bronKerboschPivot lan clique nodes seen
  | S.null nodes && S.null seen = clique
  | S.null nodes = S.empty
  | otherwise = clique'
  where
    (_, _, _, clique') = foldr bkp (clique, nodes, seen, S.empty) toConsider
    clique''
      | size clique' > size clique = clique'
      | otherwise = clique
    pivot = maximumBy (comparing (size . (lan !))) . toList . union nodes $ seen
    toConsider = difference nodes $ lan ! pivot
    bkp v (c, n, s, rv) = (c, delete v n, insert v n, rv'')
      where
        rv' =
          bronKerboschPivot
            lan
            (insert v c)
            (intersection n (lan ! v))
            (intersection seen (lan ! v))
        rv''
          | size rv' > size rv = rv'
          | otherwise = rv

findLargestInterconnected :: LAN -> String
findLargestInterconnected lan =
  intercalate ","
    . map unpack
    . toList
    . bronKerboschOrdering
        lan
        degeneracyOrdering
        (fromList . keys $ lan)
        S.empty
    $ S.empty
  where
    degeneracyOrdering =
      map fst . sortBy (comparing (length . snd)) . assocs $ lan

part1 :: Bool -> Text -> String
part1 _ =
  show
    . findTriconnectedTs
    . fromRight (error "parser error")
    . parse parseInput "day23"

part2 :: Bool -> Text -> String
part2 _ =
  findLargestInterconnected
    . fromRight (error "parser error")
    . parse parseInput "day23"
