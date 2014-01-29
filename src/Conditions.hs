{-# LANGUAGE QuasiQuotes #-}
-- | This module deals with customizing which inputs and outputs the
--   synthesis program considers.
--  
--   You can currently consider any registers (except i), some number
--   of elements from the top of either stack or some number of
--   elements in memory.
module Conditions where

import           Control.Applicative

import           Data.Char
import           Data.List

import           Text.Printf

import           SketchQQ   

data Register = A | B | P | R | S | T deriving (Show, Eq, Bounded, Enum)

data Array = Data | Ret | Memory deriving (Show, Eq, Bounded, Enum)

-- | Which parts of the state to look at for input or output.
data Condition = Register Register
               | Array Array Int -- ^ Stack or register, with the number of entries to inspect.
               deriving (Show, Eq)

-- | Displays a register or array as a suitable variable name.
name :: Condition -> String
name (Register reg) = toLower <$> show reg
name (Array arr _)  = toLower <$> show arr

-- | Produce a literal comma-separated list using some function over conditions.
list :: (Condition -> String) -> [Condition] -> String
list f = intercalate ", " . map f

-- | Produce an indented list of statements, one on each line, with
--   terminating semi-colons.
statements :: Char -> (Condition -> String) -> [Condition] -> String
statements terminator f = intercalate (terminator : "\n  ") . map f

-- | The fields of the Ret struct definition.
fields :: [Condition] -> String
fields = statements ',' go
  where go reg@Register{}     = printf "bit[BIT_SIZE] %s" $ name reg
        go arr@(Array _ size) = printf "bit[BIT_SIZE][%d] %s" size $ name arr

-- | Arguments to the sketch and spec functions.
arguments :: [Condition] -> String
arguments = list $ printf "bit[BIT_SIZE] %s_input" . name

-- | Assignments to the starting state (s).
fieldAssignments :: [Condition] -> String
fieldAssignments = statements ';' go
  where go reg@Register{}     = printf "s.%s = %s_input" (name reg) (name reg)
        go arr@(Array _ size) = printf "s.%s = %s_input[0::%d]" (name arr) (name arr) size

-- | Assignements to the returned |Ret| struct.
returnedValues :: [Condition] -> String
returnedValues = list go
  where go reg@Register{}     = printf "%s = s.%s" (name reg) (name reg)
        go arr@(Array _ size) = printf "%s = s.%s[0::%d]" (name arr) (name arr) size
