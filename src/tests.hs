{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- | Some unit tests for the Sketch implementation of F18A.
module Main where

import qualified Data.Bits as Bits
import           Data.Bits ((.&.))
import           Data.List (intercalate)

import           Conditions
import           Instrs
import           SketchQQ
import           Testing

-- | The .sk file to run all my tests.
testSk :: String
testSk = [sketch|

include "unit-tests-instrs.sk";
pragma options "--bnd-int-range 1000";

$testStr
|] where testStr = intercalate "\n\n" tests

main :: IO ()
main = do writeFile "unit-tests.sk" testSk
          writeFile "unit-tests-instrs.sk" $ instrs 18 10
          putStrLn "Generated unit-tests.sk and unit-tests-instrs.sk."

-- | A bunch of test cases, mostly taken from my array-forth
--   interpreter test suite.
tests :: [String]
tests = [

  test "1" "@p @p . + 2 3" [
     3 := p,
     5 := t,
     Unchanged [a, b, r, s]
     ],

  test "2" "@p - . . 0" [
    2 := p,
    (- 1) := t,
    Unchanged [a, b, r, s]
    ],

  test "3" "@p b! @p . 4 42 !b @p . ." [
    5 := p,
    42 := t,
    4 := b,
    42 := memory 4,
    Unchanged [a, r, s]
    ],

  test "4" "- dup dup dup dup dup dup dup" [
    2 := p,
    (- 1) := t,
    (- 1) := s,            -- fill empty [0, 0, -1, -1, -1, -1, -1, -1] := dataStack
    Unchanged [a, b, r]
    ],

  test "5" "dup or a! @p 123 !+ @p ! . 456 dup or a! . @+ 2* @+ . 2/ + ! ." [
    2 := a,
    123 := memory 0,
    456 := memory 1,
    474 := memory 2,
    7 := p,
    Unchanged [b, r, s, t]
    ],

  test "unext" ". . unext ." [
    1 := p,
    Unchanged [a, b, r, s, t]
    ],

  test "unext1" "@p push . . 41 @+ . . unext" [
    3 := p,
    42 := a,
    Unchanged [b, r, s, t]
    ],

  test "fetchP" "@p . . . 42" [
    2 := p,
    42 := t,
    Unchanged [a, b, r, s]
    ],

  test "fetchPlus" "@+ . . ." [
    1 := a,
    memory 0 := t,
    Unchanged [b, r, s]
    ],

  test "fetchB" "@b . . ." [
    memory 0 := t,
    Unchanged [b, r, s]
    ],

  test "fetch" "@ . . ." [
    memory 0 := t,
    Unchanged [b, r, s]
    ],

  test "storeP" "@p !p . . 42" [
    3 := p,
    42 := memory 2,
    Unchanged [a, b, r, s]
    ],

  test "storePlus" "@p !+ . . 42" [
    1 := a,
    2 := p,
    42 := memory 0,
    Unchanged [b, r, s]
    ],

  test "storePlus2" "@p @p a! . 42 10 !+ . . ." [
    11 := a,
    4 := p,
    42 := memory 10,
    Unchanged [b, r, s]
    ],

  test "storePlus3" "dup or a! @p 123 !+ @p . 456" [
    1 := a,
    123 := memory 0,
    456 := memory 1,
    Unchanged [b, r, s, t]
    ],

  test "storeB" "@p !b . . 42" [
    2 := p,
    42 := memory 0,
    Unchanged [a, b, r, s]
    ],

  test "storeB2" "@p @p b! . 42 10 !b . . ." [
    4 := p,
    42 := memory 10,
    10 := b,
    Unchanged [a, r, s]
    ],

  test "store" "@p ! . . 42" [
    2 := p,
    42 := memory 0,
    Unchanged [a, b, r, s]
    ],

  test "store1" "@p @p a! . 42 10 ! . . ." [
    10 := a,
    4 := p,
    42 := memory 10,
    Unchanged [b, r, s]
    ],

  test "store2" "dup or a! @p 123 ! . . ." [
    0 := a,
    123 := memory 0,
    Unchanged [b, r, s, t]
    ],

  test "times2" "@p 2* . . 2" [
    4 := t,
    2 := p,
    Unchanged [a, b, r, s]
    ],

  test "div2" "@p 2/ . . 4" [
    2 := t,
    2 := p,
    Unchanged [a, b, r, s]
    ],

  test "not" "- . . ." [
    (- 1) := t,
    1 := p,
    Unchanged [a, b, r, s]
    ],

  test "not1" "@p - . . 42" [
    Value (Bits.complement 42) := t,
    2 := p,
    Unchanged [a, b, r, s]
    ],

  test "plus" "@p @p . + 12 30" [
    42 := t,
    3 := p,
    Unchanged [a, b, r, s]
    ],

  test "and" "@p @p and . 12 30" [
    Value (12 .&. 30) := t,
    3 := p,
    Unchanged [a, b, r, s]
    ],

  test "or" "@p @p or . 12 30" [
    Value (12 `Bits.xor` 30) := t,
    3 := p,
    Unchanged [a, b, r, s]
    ],

  test "drop" "@p @p drop . 1 2" [
    1 := t,
    3 := p,
    Unchanged [a, b, r, s]
    ],

  test "dup" "@p dup . . 42" [
    42 := t,
    42 := s,
    2 := p,
    Unchanged [a, b, r]
    ],

  test "dup1" "@p dup or . 42" [
    0 := t,
    2 := p,
    Unchanged [a, b, r, s]
    ],

  test "over" "@p @p over . 1 2" [
    1 := t,
    2 := s,
    3 := p,
    Unchanged [a, b, r]
    ],

  test "a" "@p a! a . 42" [
    42 := a,
    42 := t,
    2 := p,
    Unchanged [b, r, s]
    ],

  test "nop" ". . . ." [
    1 := p,
    Unchanged [a, b, r, s, t]
    ],

  test "push" "@p push . . 42" [
    42 := r,
    2 := p,
    Unchanged [a, b, s, t]
    ],

  test "setB" "@p b! . . 42" [
    42 := b,
    2 := p,
    Unchanged [a, r, s, t]
    ],

  test "setA" "@p a! . . 42" [
    42 := a,
    2 := p,
    Unchanged [b, r, s, t]
    ]

  ]

