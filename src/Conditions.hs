{-# LANGUAGE QuasiQuotes #-}
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

fieldName :: Int -> Condition -> String
fieldName bitSize (Register reg)   =
  printf "bit[%d] %s;" bitSize $ toLower <$> show reg
fieldName bitSize (Array arr size) =
  printf "bit[%d][%d] %s;" bitSize size $ toLower <$> show arr

retStruct :: Int -> [Condition] -> String
retStruct bitSize conditions = [sketch|
struct Ret {
$fields
}
|]
  where fields = init . unlines $ ("  " ++) . fieldName bitSize <$> conditions

retStatement :: Int -> [Condition] -> String
retStatement bitSize conditions = [sketch|
return |Ret|($populations);
|]
  where populations = intercalate ", " $ populate <$> conditions

populate :: Condition -> String
populate (Register reg) =
  let name = toLower <$> show reg in printf "%s = s.%s" name name
populate (Array arr size) =
  let name = toLower <$> show arr in printf "%s = s.%s[0::%d]" name name size
