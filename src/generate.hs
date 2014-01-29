{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Options.Applicative

import           Language.ArrayForth.Program

import           Text.Printf

import           Conditions (Register (..), Condition (..))
import           GenerateSketch
import           Instrs

data GenerateSettings =
  GenerateSettings { sketchSettings :: Settings
                   , program        :: Program
                   } deriving (Show)

-- "over over or a! and a or"
main :: IO ()
main = do GenerateSettings {..} <- execParser cmdParser
          let Settings {..} = sketchSettings
          writeFile (printf "%s-sketch.sk" prefix) $ harness sketchSettings program
          writeFile (printf "%s-instrs.sk" prefix) $ instrs bits

cmdParser :: ParserInfo GenerateSettings
cmdParser = info (helper <*> options)
            ( fullDesc
           <> progDesc "Generate .sk files for the given F18A program." )

options :: Parser GenerateSettings
options = GenerateSettings <$> sketchOptions
                           <*> arguments1 instrParser
                             ( help "The specification program in F18A syntax. Use numbers as literals without the @p."
                            <> metavar "PROGRAM" )
  where instrParser program = case readInstruction program of
          Left _    -> Nothing
          Right res -> Just res

sketchOptions :: Parser Settings
sketchOptions =
  Settings <$> nullOption
             ( reader (return . map read . words)
            <> help "Which opcodes to support in the generated sketch."   
            <> long "supported"
            <> short 's'   
            <> value supported
            <> metavar "OPCODES" )
           <*> option
             ( help "Which part of the state to take as input to the sketch. Specify as a string of register names or (data|return|memory) with a number, like: \"r s t data 4 memory 10\"."
            <> long "inputs"
            <> short 'i'
            <> value [Register S, Register T]
            <> metavar "REGISTERS" )
           <*> option
             ( help "Which part of the state to take as output from the sketch. Specify as a string of register names or (data|return|memory) with a number, like: \"r s t data 4 memory 10\"."
            <> long "outputs"
            <> short 'o'
            <> value [Register S, Register T]
            <> metavar "REGISTERS" )
           <*> strOption
             ( help "A prefix for the filenames generated. This will result in filename-sketch.sk and filename-instrs.sk."
            <> long "file"
            <> short 'f'
            <> value "generated"
            <> metavar "FILENAME" )
           <*> fmap not (switch
             ( help "Do not allow holes to be filled with literal numbers."
            <> long "no-literal-holes" ))
           <*> option
             ( help "Number of holes to generate."
            <> short 'n'
            <> long "holes"
            <> value 6
            <> metavar "#_OF_HOLES" )
           <*> option
             ( help "Size of F18 words in bits."
            <> short 'b'
            <> long "bits"
            <> value 18
            <> metavar "BIT_SIZE" )
