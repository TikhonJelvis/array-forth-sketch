{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}

module GenerateSketch where

import           Data.Char
import           Data.Data
import           Data.Functor
import           Data.List

import           Language.ArrayForth.Opcode
import           Language.ArrayForth.Program

import           Text.Printf

import qualified Conditions
import           Conditions (Condition (..))
import           SketchQQ

deriving instance Typeable Opcode
deriving instance Data Opcode

-- | Settings that control how the problem gets synthesized.
data Settings =
  Settings { supportedOpcodes :: [Opcode]
           , inputs           :: [Condition]
           , outputs          :: [Condition]
           , prefix           :: FilePath  -- ^ the prefix to use to import instrs.sk
           , literalHoles     :: Bool      -- ^ allow literal numbers in sketch?
           , holes            :: Int       -- ^ number of holes in sketch
           , bits             :: Int       -- ^ number of bits in Forth words
           } deriving (Show)

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

harness :: Settings -> Program -> String
harness settings@Settings {..} spec = [sketch|

include "$prefix-instrs.sk";
pragma options "--bnd-int-range 1000";

struct Ret {
  $fields;
}
           
|Ret| spec($arguments) {
  reset();
  int step = 0;

  $assignments;
  $specProgram;
  return |Ret|($retVals);
}

|Ret| sketch($arguments) implements spec {
  reset();
  int step = 0;

  $assignments;
  $holesSk
  return |Ret|($retVals);
}

|]
  where specProgram = program bits spec
        holesSk = genHoles settings holes

        fields      = Conditions.fields outputs
        arguments   = Conditions.arguments inputs
        assignments = Conditions.fieldAssignments inputs
        retVals     = Conditions.returnedValues outputs

callOpcode :: Opcode -> String
callOpcode instr = let (f:rest) = showConstr $ toConstr instr in toLower f : rest ++ "(step)"

callLiteral :: Int -> F18Word -> String
callLiteral bitSize = printf "loadLiteral({%s}, step)" . Conditions.toBits bitSize

call :: Int -> Instruction -> String
call _ (Opcode op)      = callOpcode op
call bitSize (Number n) = callLiteral bitSize n
call _ op               = error $ "Specs with jumps, labels or holes are not supported! Instr " ++ show op ++ " is invalid"

program :: Int -> Program -> String
program bits = intercalate ";\n  " . map (\ op -> "step = " ++ call bits op)

genHoles :: Settings -> Int -> String
genHoles Settings { supportedOpcodes, literalHoles } n =
  drop 2 . unlines $ replicate n [sketch|
  step = {| $opcodeCalls $literalHole |};
|]
  where opcodeCalls = intercalate " | " $ map callOpcode supportedOpcodes
        literalHole | literalHoles = "| loadLiteral(??, step)"
                    | otherwise    = ""
