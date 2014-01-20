{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Data.Char
import           Data.Data
import           Data.Functor
import           Data.List
import           Data.String

import           Language.ArrayForth.Opcode
import           Language.ArrayForth.Parse
import           Language.ArrayForth.Program

import           Numeric

import           System.Process

import           Text.Printf

import           Sketch

instance IsString [Opcode] where fromString = map (\ (Opcode o) -> o) . read

deriving instance Typeable Opcode
deriving instance Data Opcode

supported' :: [Opcode]
supported' = [ FetchP
            , FetchPlus
            , FetchB
            , Fetch
            , StoreP
            , StorePlus
            , StoreB
            , Store
            , Times2
            , Div2
            , Not
            , Plus
            , And
            , Or
            , Drop
            , Dup
            , Pop
            , Over
            , ReadA
            , Nop
            , Push
            , SetB
            , SetA
            ]

supported = [And, Or]

main = generate "over over or a! and a or" 4

generate problem len = writeFile "generated-sketch.sk" $ harness problem len 18

harness :: Program -> Int -> Int -> String
harness spec length bits = [sketch|

include "instrs.sk";
pragma options "--bnd-int-range 1000";

struct Ret {
  bit[$bitSize] s;
  bit[$bitSize] t;
}
           
|Ret| spec(bit[$bitSize] t_reg, bit[$bitSize] s_reg) {
  reset();
  s.t = t_reg;
  s.s = s_reg;
  $specProgram;
  return |Ret|(s = s.s, t = s.t);
}

|Ret| sketch(bit[$bitSize] t_reg, bit[$bitSize] s_reg) implements spec {
  reset();
  s.t = t_reg;
  s.s = s_reg;
  bit[$bitSize] ignore = 0;
  $holes
  return |Ret|(s = s.s, t = s.t);
}

|]
  where specProgram = intercalate ";\n  " $ map (call bits) spec
        bitSize = show bits
        holes = genHoles length

callOpcode :: Opcode -> String
callOpcode instr = let (f:rest) = showConstr $ toConstr instr in toLower f : rest ++ "()"

callLiteral :: Int -> F18Word -> String
callLiteral bitSize = printf "loadLiteral(s, {%s})" . toBits
  where toBits n = intercalate "," . pad $ showIntAtBase 2 (head . show) n ""
        pad ls | length ls > bitSize = return <$> ls
               | otherwise = return <$> replicate (bitSize - length ls) '0' ++ ls

call :: Int -> Instruction -> String
call _ (Opcode op)      = callOpcode op
call bitSize (Number n) = callLiteral bitSize n

genHoles :: Int -> String
genHoles n = drop 2 . unlines $ replicate n [sketch|
  ignore = {| $opcodeCalls |};
|]
  where opcodeCalls = intercalate " | " . map callOpcode . filter isSupported $ opcodes
        isSupported = (`elem` supported')
