{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Data.Char
import           Data.Data
import           Data.String

import           Language.ArrayForth.Opcode
import           Language.ArrayForth.Parse
import           Language.ArrayForth.Program

import           Sketch

instance IsString [Opcode] where fromString = map (\ (Opcode o) -> o) . read

deriving instance Typeable Opcode
deriving instance Data Opcode

main = writeFile "inclusiveOr.sk" $ harness "over over or a! and a or" 4

harness :: [Opcode] -> Int -> String
harness spec n = [sketch|

include "instrs.sk";

struct Ret {
  bit[18] s;
  bit[18] t;
}

Ret spec(bit[18] t_reg, bit[18] s_reg) {
  reset();
  s.t = t_reg;
  s.s = s_reg;
  $specProgram;
  return new Ret(s = s.s, t = s.t);
}

Ret sketch(bit[18] t_reg, bit[18] s_reg) implements spec {
  reset();
  s.t = t_reg;
  s.s = s_reg;
  bit[18] ignore = 0;
  $holes
  return new Ret(s = s.s, t = s.t);
}

|]
  where specProgram = unlines $ map call spec
        holes = genHoles n

call instr = let (f:rest) = showConstr $ toConstr instr in toLower f : rest ++ "();"

genHoles n = unlines $ replicate n [sketch|
 ignore = {| exec() | unext() | fetchP() | fetchPlus() | fetchB() | fetch() |
    storeP() | storePlus() | storeB() | store() | times2() | div2() |
    not() | plus() | and() | or() | drop() | dup() | pop() | over() |
    readA() | nop() | push() | setB() | setA() |};
|]
