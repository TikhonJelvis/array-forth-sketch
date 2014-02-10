{-# LANGUAGE QuasiQuotes #-}
-- | A few utilities that generate test functions for our sketch code.
module Testing where

import           Data.Functor                ((<$>))
import           Data.List                   (intercalate)

import           Language.ArrayForth.Program (Program)

import           Text.Printf                 (printf)

import           Conditions
import qualified GenerateSketch
import           SketchQQ

-- | You can either assert that a particular register or memory
-- location has a particular value or that it is unchanged.
data Assert = Condition := Condition
            | Unchanged [Condition] deriving (Show, Eq)

-- | Displays an assertion as a Sketch statement.
render :: Assert -> String
render (c₁ := c₂) = printf "assert %s == %s" (toExpr c₁) (toExpr c₂)
  where toExpr reg@Register{}  = "s." ++ toName reg
        toExpr arr@(Array _ i) = printf "s.%s[%d]" (toName arr) i :: String
        toExpr (Value n)       = show n
render (Unchanged conditions) = intercalate ";\n  " $ assertUnchanged <$> conditions
  where assertUnchanged condition = render $ condition := 0

-- | Generates a test function with the given name (in the form
--   'test_<name>') that checks all the assertions against the result
--   of running the spec F18A program.
test :: String -> Program -> [Assert] -> String
test name spec assertions = [sketch|
harness void test_$name() {
  reset();
  $program;
  $conditions;
}
|]
  where program    = GenerateSketch.program 18 spec
        conditions = intercalate ";\n  " $ render <$> assertions
        
-- | Given all the test functions as a string, this wraps them up with
--   the necessary initialization code and outputs the entire contents
--   of the test.sk file.
testFile :: [String] -> String
testFile tests = [sketch|
include "test-instrs.sk";

$testCases
|]
  where testCases = intercalate "\n\n" tests
