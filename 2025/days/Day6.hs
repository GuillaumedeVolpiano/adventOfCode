{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}

module Day6
  ( part1
  , part2
  , getCorrectTotal
  , foldSheet
  , getTotal
  ) where

import           Control.DeepSeq          (NFData, rnf)
import           Data.Word                (Word8)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (foldl')
import qualified Streamly.Data.Stream     as S (fold)
import           Streamly.Data.Stream     (Stream)
import Data.Sequence (Seq((:|>), (:<|)), ViewL((:<), EmptyL))
import qualified Data.Sequence as Sq (viewl, splitAt, drop)

-- | Operators applied to columns.
--
--   Each operator also tracks how many numbers are under it in that column.
--   * 'Add n' – column to be summed, with n numbers.
--   * 'Mult n' – column to be multiplied, with n numbers.
--   * 'Null' – placeholder for initial/filler state.
data Op = Add Int | Mult Int | Null deriving (Show, Eq)

-- | Correct worksheet state: left numbers, current in-progress number, right
-- numbers.
data Correct = C !(Seq Int) {-# UNPACK #-} !Int !(Seq Int) deriving Show

-- | Wrong worksheet state for a column: accumulates sum/product per operator.
data Oped = O {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving Show

-- | Wrong worksheet state: left columns, current in-progress value, right columns.
data Wrong = W !(Seq Oped) {-# UNPACK #-} !Int !(Seq Oped) deriving Show


-- | Holds the internal parser/folder state while scanning the input stream.
--
-- Fields:
--   * Bool        – whether the previous character was a space
--   * Correct     – state of the correct worksheet for columns
--   * Wrong       – state of the wrong worksheet for columns
--   * Op          – current operator
data FoldState = FS Bool !Correct !Wrong !Op deriving Show

-- | Represents the final worksheet parsed from input.
--
-- Fields:
--   * Int – total of wrong worksheet values
--   * Int – total of correct worksheet values
data WorkSheet = WS {-# UNPACK #-} !Int {-# UNPACK #-} !Int 

instance NFData Oped where
  rnf (O a p) = rnf a `seq` rnf p

instance NFData WorkSheet where
  rnf (WS w c) = rnf w `seq` rnf c

instance NFData Op where
  rnf (Add v)  = rnf v
  rnf (Mult v) = rnf v
  rnf Null = rnf (0 :: Int)

-- | Byte constants for parsing.
pattern LF :: Word8
pattern LF = 10

pattern Space :: Word8
pattern Space = 32

pattern Times :: Word8
pattern Times = 42

pattern Plus :: Word8
pattern Plus = 43

-- | Fold function applied over the stream of input bytes.
--   Updates the FoldState according to spaces, newlines, digits, and operators.
folder :: FoldState -> Word8 -> FoldState
folder fs@(FS seenSpace correct wrong op) w
  | w == Space && op == Null = if seenSpace then slideSpace fs
                              else slideNoSpace fs
  | w == Space = FS False correct wrong (increaseOp op)
  | w ==  LF && op == Null = slideEnd fs
  | w == LF = FS False (correctCalc correct $ increaseOp op) (calc wrong op) (increaseOp op)
  | w == Times = mult fs
  | w == Plus = addition fs
  | isDigit w = addFS (digit w) fs
  | otherwise = undefined

-- | Update the wrong worksheet with the current operator.
calc :: Wrong -> Op -> Wrong
calc (W leftWrong acc ((O add _) :<| next)) (Add _) = W leftWrong (acc + add) next
calc (W leftWrong acc ((O _ prod) :<| next)) (Mult _) = W leftWrong (acc + prod) next
calc _ _ = undefined

-- | Update the correct worksheet with the current operator.
correctCalc :: Correct -> Op -> Correct
correctCalc (C leftCorrect acc rightCorrect) op = C leftCorrect acc' next'
  where
    l = case op of
          Add v -> v
          Mult v -> v
          Null -> undefined
    (cur, next) = Sq.splitAt l rightCorrect
    next' = Sq.drop 1 next
    acc' = case op of
             Add _ -> acc + sum cur
             Mult _ -> acc + product cur
             Null -> undefined

-- | Set multiplication operator in FoldState.
mult :: FoldState -> FoldState
mult (FS seenSpace correct@(C leftCorrect _ rightCorrect) wrong@(W leftWrong _ rightWrong) op)
  | op == Null = FS seenSpace (C leftCorrect 0 rightCorrect) (W leftWrong 0 rightWrong) (Mult 0) 
  | otherwise = FS seenSpace (correctCalc correct op) (calc wrong op) (Mult 0)

-- | Set addition operator in FoldState.
addition :: FoldState -> FoldState
addition (FS seenSpace correct@(C leftCorrect _ rightCorrect) wrong@(W leftWrong _ rightWrong) op)
  | op == Null = FS seenSpace (C leftCorrect 0 rightCorrect) (W leftWrong 0 rightWrong) (Add 0)
  | otherwise = FS seenSpace (correctCalc correct op) (calc wrong op) (Add 0)


-- | Handle end-of-line: shift columns and reset current counters.
slideEnd :: FoldState -> FoldState
slideEnd (FS seenSpace correct wrong@(W leftWrong _ _) ops)
  | seenSpace = FS False (C mempty (-1) lc) (W mempty 0 leftWrong) ops
  | otherwise = FS False (C mempty (-1) lc) (W mempty 0 lw) ops
  where
    (C lc _ _) = slideCorrect correct
    (W lw _ _) = slideWrong wrong

-- | Add a digit to the current column in the state.
addFS :: Int -> FoldState -> FoldState
addFS dig (FS _ correct (W leftWrong wrong rightWrong) ops) =
  FS False correct' (W leftWrong (10*wrong + dig) rightWrong) ops
    where
      correct' = addCorrect dig correct
{-# INLINE addFS #-}

-- | Update the correct worksheet state when a digit is added.
addCorrect :: Int -> Correct -> Correct
addCorrect !dig correct = C lc (10*c + dig) rc
  where
    (C lc c rc) = slideCorrect correct
{-# INLINE addCorrect #-}

-- | Handle a space character when parsing correct numbers.
slideSpace :: FoldState -> FoldState
slideSpace (FS _ correct wrong ops) =
  FS True correct' wrong ops
  where
    correct' = slideCorrect correct
{-# INLINE slideSpace #-}

-- | Shift the current correct value into leftCorrect and advance the
-- rightCorrect cursor.
slideCorrect :: Correct -> Correct
slideCorrect (C !leftCorrect !correct !rightCorrect)
  | correct < 0 = case Sq.viewl rightCorrect of
                    EmptyL -> C leftCorrect 0 rightCorrect
                    (c' :< rest) -> C leftCorrect c' rest
  | otherwise = case Sq.viewl rightCorrect of
                  EmptyL -> C (leftCorrect :|> correct) 0 rightCorrect
                  (c' :< rest) -> C (leftCorrect :|> correct) c' rest
{-# INLINE slideCorrect #-}


-- | Shift the current wrong value into leftWrong and advance the rightWrong
-- cursor.
slideWrong :: Wrong -> Wrong
slideWrong (W !leftWrong !wrong !rightWrong)
  | wrong == 0 = W leftWrong 0 rightWrong
  | otherwise = case Sq.viewl rightWrong of
                  EmptyL -> W (leftWrong :|> O wrong wrong) 0 rightWrong
                  ((O add prod) :< rest) -> W (leftWrong :|> O (wrong + add) (wrong * prod)) 0 rest
{-# INLINE slideWrong #-}

-- | Increase the number of numbers counted for the last operator.
increaseOp :: Op -> Op
increaseOp (Add v) = Add (v + 1)
increaseOp (Mult v) = Mult (v + 1)
increaseOp _ = undefined
{-# INLINE increaseOp #-}

-- | Handle a space when no previous space was seen.
slideNoSpace :: FoldState -> FoldState
slideNoSpace (FS _ correct wrong ops) = FS True correct' wrong' ops
  where
    correct' = slideCorrect correct
    wrong' = slideWrong wrong
{-# INLINE slideNoSpace #-}

-- | Fold a stream of Word8 into a WorkSheet.
foldSheet :: Stream IO Word8 -> IO WorkSheet
foldSheet = fmap (\(FS _ (C _ correct _) (W _ wrong _) _ ) -> WS wrong correct) .
  S.fold (F.foldl' folder (FS False (C mempty (-1) mempty) (W mempty 0 mempty) Null))

-- | Compute the total for the wrong worksheet values
getTotal :: WorkSheet -> Int
getTotal (WS w _) = w

-- | Compute the total for using correct worksheet values.
getCorrectTotal :: WorkSheet -> Int
getCorrectTotal (WS _ c) = c

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = foldSheet s >>= print . getTotal

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = foldSheet s >>= print . getCorrectTotal
