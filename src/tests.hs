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
          writeFile "unit-tests-instrs.sk" $ program 18 15
          putStrLn "Generated unit-tests.sk and unit-tests-instrs.sk."

-- | A bunch of test cases, mostly taken from my array-forth
--   interpreter test suite.
tests :: [String]
tests = [

  test "1" "@p @p . + 2 3" [
     p := 3,
     t := 5,
     Unchanged [a, b, r, s]
     ],

  test "2" "@p - . . 0" [
    p := 2,
    t := (- 1),
    Unchanged [a, b, r, s]
    ],

  test "3" "@p b! @p . 4 42 !b @p . ." [
    p := 5,
    t := 42,
    b := 4,
    Unchanged [a, r, s]
    ],

  test "4" "- dup dup dup dup dup dup dup" [
    p := 2,
    t := (- 1),
    s := (- 1),     -- fill empty [0, 0, -1, -1, -1, -1, -1, -1] := dataStack
    Unchanged [a, b, r]
    ],

  test "5" "dup or a! @p 123 !+ @p ! . 456 dup or a! . @+ 2* @+ . 2/ + ! ." [
    a := 2,
    memory 0 := 123,
    memory 1 := 456,
    memory 2 := 474,
    p := 7,
    Unchanged [b, r, s, t]
    ],

  test "unext" ". . unext ." [
    p := 1,
    Unchanged [a, b, r, s, t]
    ],

  test "unext1" "@p push . . 41 @+ . . unext" [
    p := 3,
    a := 42,
    Unchanged [b, r, s, t]
    ],

  test "fetchP" "@p . . . 42" [
    p := 2,
    t := 42,
    Unchanged [a, b, r, s]
    ],

  test "fetchPlus" "@+ . . ." [
    a := 1,
    t := 0,
    Unchanged [b, r, s]
    ],

  test "fetchB" "@b . . ." [
    t := 0,
    Unchanged [b, r, s]
    ],

  test "fetch" "@ . . ." [
    t := 0,
    Unchanged [b, r, s]
    ],

  test "storeP" "@p !p . . 42" [
    p := 3,
    memory 2 := 42,
    Unchanged [a, b, r, s]
    ],

  test "storePlus" "@p !+ . . 42" [
    a := 1,
    p := 2,
    memory 0 := 42,
    Unchanged [b, r, s]
    ],

  test "storePlus2" "@p @p a! . 42 10 !+ . . ." [
    a := 11,
    p := 4,
    memory 10 := 42,
    Unchanged [b, r, s]
    ],

  test "storePlus3" "dup or a! @p 123 !+ @p . . 456" [
    a := 1,
    memory 0 := 123,
    memory 1 := 456,
    Unchanged [b, r, s, t]
    ],

  test "storeB" "@p !b . . 42" [
    p := 2,
    memory 0 := 42,
    Unchanged [a, b, r, s]
    ],

  test "storeB2" "@p @p b! . 42 10 !b . . ." [
    p := 4,
    memory 10 := 42,
    b := 10,
    Unchanged [a, r, s]
    ],

  test "store" "@p ! . . 42" [
    p := 2,
    memory 0 := 42,
    Unchanged [a, b, r, s]
    ],

  test "store1" "@p @p a! . 42 10 ! . . ." [
    a := 10,
    p := 4,
    memory 10 := 42,
    Unchanged [b, r, s]
    ],

  test "store2" "dup or a! @p 123 ! . . ." [
    a := 0,
    memory 0 := 123,
    Unchanged [b, r, s, t]
    ],

  test "times2" "@p 2* . . 2" [
    t := 4,
    p := 2,
    Unchanged [a, b, r, s]
    ],

  test "div2" "@p 2/ . . 4" [
    t := 2,
    p := 2,
    Unchanged [a, b, r, s]
    ],

  test "not" "- . . ." [
    t := (- 1),
    p := 1,
    Unchanged [a, b, r, s]
    ],

  test "not1" "@p - . . 42" [
    Value (Bits.complement 42) := t,
    p := 2,
    Unchanged [a, b, r, s]
    ],

  test "plus" "@p @p . + 12 30" [
    t := 42,
    p := 3,
    Unchanged [a, b, r, s]
    ],

  test "and" "@p @p and . 12 30" [
    Value (12 .&. 30) := t,
    p := 3,
    Unchanged [a, b, r, s]
    ],

  test "or" "@p @p or . 12 30" [
    Value (12 `Bits.xor` 30) := t,
    p := 3,
    Unchanged [a, b, r, s]
    ],

  test "drop" "@p @p drop . 1 2" [
    t := 1,
    p := 3,
    Unchanged [a, b, r, s]
    ],

  test "dup" "@p dup . . 42" [
    t := 42,
    s := 42,
    p := 2,
    Unchanged [a, b, r]
    ],

  test "dup1" "@p dup or . 42" [
    t := 0,
    p := 2,
    Unchanged [a, b, r, s]
    ],

  test "over" "@p @p over . 1 2" [
    t := 1,
    s := 2,
    p := 3,
    Unchanged [a, b, r]
    ],

  test "a" "@p a! a . 42" [
    a := 42,
    t := 42,
    p := 2,
    Unchanged [b, r, s]
    ],

  test "nop" ". . . ." [
    p := 1,
    Unchanged [a, b, r, s, t]
    ],

  test "push" "@p push . . 42" [
    r := 42,
    p := 2,
    Unchanged [a, b, s, t]
    ],

  test "setB" "@p b! . . 42" [
    b := 42,
    p := 2,
    Unchanged [a, r, s, t]
    ],

  test "setA" "@p a! . . 42" [
    a := 42,
    p := 2,
    Unchanged [b, r, s, t]
    ]

  ]

