{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
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

-- | Settings that control how the problem gets synthesized.
data Settings =
  Settings { supportedOpcodes :: [Opcode]
           , literalHoles     :: Bool      -- ^ allow literal numbers in sketch?
           , holes            :: Int       -- ^ number of holes in sketch
           , bits             :: Int       -- ^ number of bits in Forth words
           }
-- IMPORTANT: Right now, you have to manually update instrs.sk with the bit size!

defaults :: Settings
defaults = Settings { supportedOpcodes = supported
                    , literalHoles     = True
                    , holes            = 6
                    , bits             = 18
                    }

-- | The usual set of supported instructions. Pretty much anything
--   without a literal address encoded in the instruction.
-- 
--   MultiplyStep is also not supported, but it should be in the
--   future.
supported :: [Opcode]
supported = [ FetchP
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

-- "over over or a! and a or"
main :: IO ()
main = writeFile "generated-sketch.sk" $ harness settings "3 10 +"
  where settings = defaults { holes = 1 } -- this is how you can "modify" the default settings

harness :: Settings -> Program -> String
harness settings@Settings { holes, bits } spec = [sketch|

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
  $holesSk
  return |Ret|(s = s.s, t = s.t);
}

|]
  where specProgram = intercalate ";\n  " $ map (call bits) spec
        bitSize = show bits
        holesSk = genHoles settings holes

callOpcode :: Opcode -> String
callOpcode instr = let (f:rest) = showConstr $ toConstr instr in toLower f : rest ++ "()"

callLiteral :: Int -> F18Word -> String
callLiteral bitSize = printf "loadLiteral({%s})" . toBits
  where toBits n = intercalate "," . reverse . pad $ showIntAtBase 2 (head . show) n ""
        pad ls | length ls > bitSize = return <$> ls
               | otherwise = return <$> replicate (bitSize - length ls) '0' ++ ls

call :: Int -> Instruction -> String
call _ (Opcode op)      = callOpcode op
call bitSize (Number n) = callLiteral bitSize n

genHoles :: Settings -> Int -> String
genHoles Settings { supportedOpcodes, literalHoles } n =
  drop 2 . unlines $ replicate n [sketch|
  ignore = {| $opcodeCalls $literalHole |};
|]
  where opcodeCalls = intercalate " | " $ map callOpcode supportedOpcodes
        literalHole | literalHoles = " | loadLiteral(??)"
                    | otherwise    = ""
