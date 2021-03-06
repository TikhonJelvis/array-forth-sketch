{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TupleSections        #-}

-- | This module deals with customizing which inputs and outputs the
--   synthesis program considers.
--  
--   You can currently consider any registers (except i), some number
--   of elements from the top of either stack or some number of
--   elements in memory.
module Conditions where

import           Control.Applicative

import           Data.Char
import           Data.Maybe
import           Data.List

import           Language.ArrayForth.Opcode (F18Word)

import           Numeric

import           Text.Printf

data Register = A | B | P | R | S | T deriving (Show, Eq, Bounded, Enum)

instance Read Register where
  readsPrec _ (name:rest) | Just reg <- register = [(reg, rest)]
    where register = find ((== name) . toLower . head . show) $ [A ..] 
  readsPrec _ _ = []

data Array = Data | Ret | Memory deriving (Show, Eq, Bounded, Enum)

instance Read Array where
  readsPrec _ str | Just (arr, rest) <- array = [(arr, rest)]
    where array = (, dropWhile (/= ' ') str) <$> (toArray =<< listToMaybe (words str))
          toArray name = find ((== name) . show) $ [Data ..]
  readsPrec _ _ = []

-- | Which parts of the state to look at for input or output.
data Condition = Register Register
               | Array Array Int -- ^ Stack or register, with the number of entries to inspect.
               | Value F18Word   -- ^ A literal number. This is not valid everywhere.
               deriving (Show, Eq)

instance Read Condition where
  readsPrec prec str
    | [reg] <- words str          = [(Register $ read reg, "")]
    | name:size:rest <- words str =
      case readsPrec prec name of
        (reg, rest'):_ -> [(Register reg, unwords $ rest':size:rest)]
        []            -> [(Array (read name) (read size), unwords rest)]
  readsPrec _ _ = []

instance Read [Condition] where
  readsPrec prec str = case readsPrec prec str of
    (condition, rest):_ -> combine condition $ readsPrec prec rest
    []                  -> []
    where combine x [] = [([x], "")]
          combine x ((xs, str'):_) = [(x : xs, str')]

instance Num Condition where
  fromInteger = Value . fromInteger
  (+) = binOp (+)
  (-) = binOp (-)
  (*) = binOp (*)

binOp :: (F18Word -> F18Word -> F18Word) -> (Condition -> Condition -> Condition)
binOp (⊕) (Value n₁) (Value n₂) = Value $ n₁ ⊕ n₂
binOp _ _ _                     = error "Cannot do numeric options on non-literals."                                  

a, b, p, r, s, t :: Condition
[a, b, p, r, s, t] = Register <$> [A ..]

data', ret, memory :: Int -> Condition
[data', ret, memory] = Array <$> [Data ..]

-- | Display a number as a comma-delimited list of bits, with the
--   least significant bit first.
toBits :: (Integral a, Show a) => Int -> a -> String
toBits bitSize n = intercalate "," . reverse . pad $ showIntAtBase 2 (head . show) n ""
  where pad ls | length ls > bitSize = return <$> ls
               | otherwise = return <$> replicate (bitSize - length ls) '0' ++ ls


-- | Displays a register or array as a suitable variable name.
toName :: Condition -> String
toName (Register reg) = toLower <$> show reg
toName (Array arr _)  = toLower <$> show arr
toName (Value n)      = printf "{%s}" $ toBits 18 n

-- | Produce a literal comma-separated list using some function over conditions.
toList :: (Condition -> String) -> [Condition] -> String
toList f = intercalate ", " . map f

-- | Produce an indented list of statements, one on each line, with
--   terminating semi-colons.
toStatements :: (Condition -> String) -> [Condition] -> String
toStatements f = intercalate (";\n  ") . map f

-- | The fields of the Ret struct definition.
fields :: [Condition] -> String
fields = toStatements go
  where go reg@Register{}     = printf "bit[BIT_SIZE] %s" $ toName reg
        go arr@(Array _ size) = printf "bit[BIT_SIZE][%d] %s" size $ toName arr
        go Value{}            = error "Cannot have a literal number as a field!"

-- | Arguments to the sketch and spec functions.
arguments :: [Condition] -> String
arguments = toList $ printf "bit[BIT_SIZE] %s_input" . toName

-- | Assignments to the starting state (s).
fieldAssignments :: [Condition] -> String
fieldAssignments = toStatements go
  where go reg@Register{}          = printf "s.%s = %s_input" (toName reg) (toName reg)
        go arr@(Array Memory size) = printf "s.%s = %s_input[0::%d]" (toName arr) (toName arr) size
        go arr@(Array _ size)      = printf "s.%s.body = %s_input[0::%d]" (toName arr) (toName arr) size
        go Value{}                 = error "Cannot use a literal value as a field!"

-- | Assignements to the returned |Ret| struct.
returnedValues :: [Condition] -> String
returnedValues = toList go
  where go reg@Register{}          = printf "%s = s.%s" (toName reg) (toName reg)
        go arr@(Array Memory size) = printf "%s = s.%s[0::%d]" (toName arr) (toName arr) size
        go arr@(Array _ size)      = printf "%s = s.%s.body[0::%d]" (toName arr) (toName arr) size
        go Value{}                 = error "Cannot use a literal value as a field!"
