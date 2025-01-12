module Day24
  ( part1
  , part2
  ) where

import           Control.Monad.State.Lazy   (State, evalState, get, put,
                                             runState)
import           Data.Bifunctor             (first)
import           Data.Bits                  (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString            (ByteString, pack)
import           Data.Char                  (chr, ord)
import           Data.Either                (fromRight)
import           Data.IntMap.Strict         (IntMap, elems, empty, fromList,
                                             insert, insertWith, keys, member,
                                             notMember, restrictKeys, size, (!))
import qualified Data.IntMap.Strict         as M (filter, lookup)
import           Data.IntSet                (IntSet)
import qualified Data.IntSet                as S (empty, insert, member,
                                                  notMember)
import           Data.List                  (intercalate, partition, sort,
                                             sortBy, (\\))
import           Data.Maybe                 (catMaybes)
import           Data.Word                  (Word8)
import           Data.Word8                 (_0, _A, _D, _N, _O, _R, _X, _colon,
                                             _greater, _hyphen, _space, _x, _z)
import           Helpers.Parsers.ByteString (Parser)
import           Text.Megaparsec            (eof, manyTill, parse, (<|>))
import           Text.Megaparsec.Byte       (alphaNumChar, char, eol, string)
import           Text.Megaparsec.Byte.Lexer (decimal)

type Register = IntMap Bool

data Op
  = AND
  | OR
  | XOR
  deriving (Show, Eq, Ord)

type Pre = (Int, Op)

type Suc = Int

type Gates = IntMap ([Pre], [Suc])

data ArticulationsState =
  AS Gates IntSet (IntMap Int) (IntMap Int) (IntMap Int) (IntMap Int)

type FullAdders = IntMap Gates

-- helper functions to manipulate the gates (encode and decode them, insert them
-- into an IntMap, sort them)
opify :: [Word8] -> Op
opify op
  | op == [_A, _N, _D] = AND
  | op == [_O, _R] = OR
  | op == [_X, _O, _R] = XOR

insertGates :: Int -> Op -> Int -> Int -> Gates -> Gates
insertGates dest op cell1 cell2 =
  insertWith
    (\(a, _) (c, d) -> (a ++ c, d))
    dest
    ([(cell1, op), (cell2, op)], [])
    . insertWith (\(_, b) (c, d) -> (c, b ++ d)) cell1 ([], [dest])
    . insertWith (\(_, b) (c, d) -> (c, b ++ d)) cell2 ([], [dest])

encode :: [Word8] -> Int
encode = foldr (\c -> (+ fromIntegral c) . flip shiftL 7) 0

decode :: Int -> String
decode 0 = ""
decode i = chr (i .&. 127) : (decode . shiftR i $ 7)

compareCoded :: Int -> Int -> Ordering
compareCoded a b =
  compare (a .&. 127) (b .&. 127)
    `mappend` compare (a' .&. 127) (b' .&. 127)
    `mappend` compare a b
  where
    a' = shiftR a 7
    b' = shiftR b 7

isChar :: Char -> Int -> Bool
isChar c i = ord c == i .&. 127

fromBin :: Bool -> Int -> Int
fromBin b i =
  shiftL i 1
    .|. (if b
           then 1
           else 0)

-- a Full adder is made of two input cells (xAA and yAA), an output cell, zAA,
-- a carry in cell and a carry out cell and a carry out cell. For z00, there is
-- no Carry In cell, and the carry out cell is directly the and of x00 and y00.
-- For z>z00, the Carry in cell is the carry out of the previous adder, and
-- should be connected directly to zAA.
-- The carry in is compared to the xor of xaa and yaa via xor, and gives the
-- value of zAA.
-- To find the carry out, the adder takes the and of (xAA xor yAA) and of the
-- carry in, and compares it via either or or xor with the and of xaa and yaa.
-- The last z is the last carry out.
-- So, assuming that the faulty gates are all between z01 and z44, we can check
-- whether each series of gates is of the form
-- z = g1 xor cin
-- g1 = x xor y
-- cout = g2 or g3
-- g2 = cin and g1
-- g3 = x and y
-- First, we need to rebuild our gates as a directional graph. Each node has 0
-- or 2 in edges, and 0, 1 or 2 out edges. In particular, zs have 2 in edges and
-- 0 out edges; xs and ys have 0 in edges and 2 out edges; all others have 1 out
-- edge and 2 in edges
-- First, we parse the input into the original register and the Gates. Each Gate
-- has 0 or 2 in links and 0 or 1 out link.
parseInput :: Parser (Register, Gates)
parseInput = do
  register <- parseRegister
  gates <- parseGates
  return (register, gates)

parseGates :: Parser Gates
parseGates =
  parseGate <|> do
    eof
    return empty

parseGate :: Parser Gates
parseGate = do
  cell1 <- encode <$> manyTill alphaNumChar (char _space)
  op <- opify <$> manyTill alphaNumChar (char _space)
  cell2 <-
    encode
      <$> manyTill
            alphaNumChar
            (string . pack $ [_space, _hyphen, _greater, _space])
  dest <- encode <$> manyTill alphaNumChar eol
  insertGates dest op cell1 cell2 <$> parseGates

parseRegister :: Parser Register
parseRegister =
  parseCell <|> do
    eol
    return empty

parseCell :: Parser Register
parseCell = do
  cell <- encode <$> manyTill alphaNumChar (string . pack $ [_colon, _space])
  value <- (== 1) <$> decimal
  eol
  insert cell value <$> parseRegister

-- Now we find the articulation points of our graph, treating it as undirected
-- and starting from  x00
articulations :: Gates -> [Int]
articulations gates =
  evalState (flip findArticulations 0 . encode $ [_x, _0, _0])
    $ AS gates S.empty empty empty empty empty

findArticulations :: Int -> Int -> State ArticulationsState [Int]
findArticulations toSee d = do
  state@(AS gates _ _ _ _ _) <- prepare toSee d <$> get
  let neighbours = uncurry (++) . first (map fst) $ gates ! toSee
      (aps, state'@(AS _ _ depth low children parent), isArt) =
        foldr (dive toSee d) ([], state, False) neighbours
      aps'
        | (toSee `notMember` parent && children ! toSee > 1)
            || (toSee `member` parent && isArt) = toSee : aps
        | otherwise = aps
  put state'
  return aps'

prepare :: Int -> Int -> ArticulationsState -> ArticulationsState
prepare toSee d (AS gates seen depth low children parent) =
  AS
    gates
    (S.insert toSee seen)
    (insert toSee d depth)
    (insert toSee d low)
    (insert toSee 0 children)
    parent

dive ::
     Int
  -> Int
  -> Int
  -> ([Int], ArticulationsState, Bool)
  -> ([Int], ArticulationsState, Bool)
dive prev d cur (aps, state@(AS gates seen depth low children parent), isArt)
  | cur `S.member` seen && M.lookup prev parent == Just cur =
    (aps, state, isArt)
  | cur `S.member` seen =
    ( aps
    , AS
        gates
        seen
        depth
        (insert prev (min (low ! prev) (depth ! cur)) low)
        children
        parent
    , isArt)
  | otherwise = (aps' ++ aps, state'', isArt')
  where
    state' = AS gates seen depth low children' parent'
    (aps', AS _ seen' depth' low' children'' parent'') =
      runState (findArticulations cur (d + 1)) state'
    lowprev = min (low' ! prev) (low' ! cur)
    curChild = children ! prev
    children' = insert prev (curChild + 1) children
    parent' = insert cur prev parent
    state'' =
      AS gates seen' depth' (insert prev lowprev low') children'' parent''
    isArt'
      | low' ! cur >= depth' ! prev = True
      | otherwise = isArt

-- Once we have done that, we can split our graph into weakly connected subgraphs, one per bit.
-- These are our uncorrected full adders. To do that,  we do a depth first
-- search, starting from xAA and considering articulation points as endpoints.
-- We don't assume a minimal and a maximal value for our bits, for the sake of
-- generality
splitAdder :: Gates -> FullAdders
splitAdder gates = fromList . foldr split [] $ startPoints
  where
    arts = articulations gates
    startPoints = filter (isChar 'x') . keys $ gates
    split startPoint =
      (:) (read . tail . decode $ startPoint, weakGraph startPoint)
    weakGraph = restrictKeys gates . flip weaklyConnected S.empty
    weaklyConnected point reachables
      | point `elem` arts || null nexts = S.insert point reachables
      | otherwise = foldr weaklyConnected (S.insert point reachables) nexts
      where
        nexts =
          filter (`S.notMember` reachables) . uncurry (++) . first (map fst)
            $ gates ! point

-- As we have seen, a subgraph (except for 00 and 45) should have three in gates
-- (cin, x and y), two out gates (z and cout) and 3 intermediary gates, for a
-- total of 8 gates. Do we have any subgraph which is smaller than that ?
findSmallSubs :: FullAdders -> FullAdders
findSmallSubs = M.filter ((< 8) . size)

-- Do we have any gate that hasn't been included in any subgraphs ?
findOutlyingGates :: Gates -> FullAdders -> [Int]
findOutlyingGates gates adders = keys gates \\ (concatMap keys . elems $ adders)

-- A FullAdder is correct if it gives the correct result both in the z cell and
-- in the cout cell (first and second member of the tuple). These tests do not
-- work for the first adder (cin will not be found) nor for the last adder (cout
-- will not be found). If we are testing the first adder, it will not have a
-- carry in. If we are testing the last adder, the carryOut will in fact be a z
-- cell.
checkAdder :: Gates -> Bool
checkAdder gates
  | hasCin gates = testAdder halfAdderTests && testAdder carryTests
  | z == encode [_z, _0, _0] = testAdder zeroHalfAdderTests
  | otherwise = False
  where
    gateNames = keys gates
    x = head . filter (isChar 'x') $ gateNames
    y = head . filter (isChar 'y') $ gateNames
    (z, cout)
      | length zs == 2 = (\[a, b] -> (a, b)) . sortBy compareCoded $ zs
      | otherwise = (head zs, cOut gates)
    zs = filter (isChar 'z') gateNames
    cin = cIn gates
    zeroHalfAdderTests =
      [([(x, True), (y, yB)], (not yB, yB)) | yB <- [False, True]]
    halfAdderTests =
      [([(cin, False), (x, True), (y, yB)], (not yB, yB)) | yB <- [False, True]]
    carryTests =
      [ ([(cin, True), (x, xB), (y, yB)], (xB == yB, xB || yB))
      | xB <- [False, True]
      , yB <- [False, True]
      , (xB, yB) /= (False, True)
      ]
    testAdder = all isCorrect
    isCorrect (ins, (zr, cor)) = register' ! z == zr && register' ! cout == cor
      where
        register' = evalGates (fromList ins, prunedGates)
    -- we need to prune our cin (if there is one) and cout so that they don't
    -- point outwith the adder
    prunedGates
      | hasCin gates =
        insert cin ([], snd (gates ! cin))
          . insert cout (fst (gates ! cout), [])
          $ gates
      | otherwise = insert cout (fst (gates ! cout), []) gates

cIn :: Gates -> Int
cIn gates =
  head . filter (any ((`notElem` gateNames) . fst) . fst . (gates !))
    $ gateNames
  where
    gateNames = keys gates

cOut :: Gates -> Int
cOut gates =
  head . filter (any (`notElem` gateNames) . snd . (gates !)) $ gateNames
  where
    gateNames = keys gates

hasCin :: Gates -> Bool
hasCin gates =
  any (any ((`notElem` gateNames) . fst) . fst . (gates !)) gateNames
  where
    gateNames = keys gates

-- Now we have all the tools we need. An outlier adder can either be
-- characterised by the fact that it is too small or that it doesn't give the
-- right result. If it's too small, then we need to add one of the outlying
-- gates. If it doesn't give the right result, then we need to swap gates within
-- the faulty adder. We can find the gates that need swapping by identifying
-- their position within the adder and checking whether the op they are using is
-- correct.
-- First, we find the outliers
findOutliers :: Gates -> String
findOutliers gates =
  intercalate ","
    . sort
    . concatMap (fixOutlier gates adders)
    . elems
    . M.filter (not . checkAdder)
    $ adders
  where
    adders = splitAdder gates

-- for a misbehaving adder, either they do not have a cin (in which case we just
-- need to connect z to the cout of the previous adder, or the problem lies
-- within the adder itself). A clear error will be when z is not connected via
-- xor.
fixOutlier :: Gates -> FullAdders -> Gates -> [String]
fixOutlier gates adders adder
  | hasCin adder && unconnectedCin = [decode z, toCin]
  | hasCin adder && length toSwap > 2 =
    error ("can't decide between " ++ (intercalate ", " . map decode $ toSwap))
  | hasCin adder = map decode toSwap
  | z == encode [_z, _0, _0] = error "implement fixer for z00"
  | (== 2) . length . filter (isChar 'z') $ gateNames =
    error ("implement fixer for last bits " ++ (tail . decode $ z))
  | otherwise = [decode z, toCout]
  where
    gateNames = keys adder
    z = head . filter (isChar 'z') $ gateNames
    -- used if gates has no Cin
    cur = read . tail . decode $ z
    prev = cur - 1
    prevGates = adders ! prev
    prevNames = keys prevGates
    coutPrev =
      head . filter (any (`notElem` prevNames) . snd . (prevGates !))
        $ prevNames
    toOuts = snd $ prevGates ! coutPrev
    toCout = decode . head . filter ((== XOR) . opGate) $ toOuts
    -- used if gates has Cin
    cin = cIn adder
    cout = cOut adder
    -- used if z is not connected to Cin
    unconnectedCin = notElem z . snd $ adder ! cin
    toIns = snd $ adder ! cin
    toCin = decode . head . filter ((== XOR) . opGate) $ toIns
    -- used otherwise
    x = head . filter (isChar 'x') $ gateNames
    g1 = head . filter (/= cin) . map fst . fst $ adder ! z
    g3 = head . filter (/= g1) . snd $ adder ! x
    g2 = head . filter (/= z) . snd $ adder ! cin
    opGate = snd . head . fst . (gates !)
    eZ
      | opGate z == XOR = Nothing
      | otherwise = Just z
    eG1
      | opGate g1 == XOR = Nothing
      | otherwise = Just g1
    eG2
      | opGate g2 == AND = Nothing
      | otherwise = Just g2
    eG3
      | opGate g3 == AND = Nothing
      | otherwise = Just g3
    eCout
      | opGate cout == OR = Nothing
      | otherwise = Just cout
    toSwap = catMaybes [eZ, eG1, eG2, eG3, eCout]

evalGates :: (Register, Gates) -> Register
evalGates (register, gates) = calcNext register nextKeys notYet
  where
    initialKeys = keys register
    (nextKeys, notYet) =
      partition (isReady register) . concatMap (snd . (gates !)) $ initialKeys
    isReady r = all ((`member` r) . fst) . fst . (!) gates
    calcNext r ks rest
      | null ks = r
      | otherwise = calcNext r' ks' rest'
      where
        r' = foldr calcOne r ks
        comingNext = (++ rest) . concatMap (snd . (gates !)) $ ks
        (ks', rest') = partition (isReady r') comingNext
    calcOne k r = insert k (op (r ! g1) (r ! g2)) r
      where
        [(g1, rop), (g2, _)] = fst $ gates ! k
        op
          | rop == AND = (&&)
          | rop == XOR = (/=)
          | rop == OR = (||)

calcNumber :: (Register, Gates) -> Int
calcNumber = extract . evalGates
  where
    zs = sortBy compareCoded . filter (isChar 'z') . keys
    extract r = foldr (fromBin . (r !)) 0 . zs $ r

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . calcNumber
    . fromRight (error "parser error")
    . parse parseInput "day24"

part2 :: Bool -> ByteString -> String
part2 _
--  unlines
--    . searchFix
 =
  show
    . findOutliers
    . snd
    . fromRight (error "parser error")
    . parse parseInput "day24"
