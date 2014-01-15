{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Data.Char
import           Data.Data
import           Data.List
import           Data.String

import           Language.ArrayForth.Opcode
import           Language.ArrayForth.Parse
import           Language.ArrayForth.Program

import           System.Process

import           Sketch

instance IsString [Opcode] where fromString = map (\ (Opcode o) -> o) . read

deriving instance Typeable Opcode
deriving instance Data Opcode

main = do writeFile "generated-sketch.sk" $ harness "over over or a! and a or" 4
          -- result <- readProcess "./sketch" ["generated-sketch.sk"] ""
          -- print $ parseSk result

parseSk = map (readInstr . takeWhile isLetter . dropWhile (not . isLetter)) .
          takeWhile (not . isInfixOf "for") .
          filter (not . isInfixOf "bit[18]") .
          drop 7 .
          takeWhile (/= "}") .
          dropWhile (not . isPrefixOf "void sketch") .
          lines
  where readInstr = readConstr $ dataTypeOf Or

harness :: [Opcode] -> Int -> String
harness spec n = [sketch|

include "instrs.sk";
pragma options "--bnd-int-range 1000";

struct Ret {
  bit[18] s;
  bit[18] t;
}
           
|Ret| spec(bit[18] t_reg, bit[18] s_reg) {
  reset();
  s.t = t_reg;
  s.s = s_reg;
  $specProgram;
  return |Ret|(s = s.s, t = s.t);
}

|Ret| sketch(bit[18] t_reg, bit[18] s_reg) implements spec {
  reset();
  s.t = t_reg;
  s.s = s_reg;
  bit[18] ignore = 0;
  $holes
  return |Ret|(s = s.s, t = s.t);
}

|]
  where specProgram = intercalate ";\n  " $ map call spec
        holes = genHoles n

call instr = let (f:rest) = showConstr $ toConstr instr in toLower f : rest ++ "()"

genHoles n = drop 2 . unlines $ replicate n [sketch|
  ignore = {| $calls |};
|]
  where calls = intercalate " | " . map call . filter supported $  opcodes
        supported opcode = not (isJump opcode) && opcode /= MultiplyStep && opcode /= Ret
