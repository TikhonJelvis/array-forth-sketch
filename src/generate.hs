{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Options.Applicative

import           Language.ArrayForth.Program

import           GenerateSketch

data GenerateSettings =
  GenerateSettings { sketchSettings :: Settings
                   , outFileName    :: FilePath
                   , program        :: Program
                   }

-- "over over or a! and a or"
main :: IO ()
main = do GenerateSettings {..} <- execParser cmdParser
          writeFile outFileName $ harness sketchSettings program
  where settings = defaults { holes = 1 } -- this is how you can "modify" the default settings

cmdParser :: ParserInfo GenerateSettings
cmdParser = info (helper <*> options)
            ( fullDesc
           <> progDesc "Generate .sk files for the given F18A program." )

options :: Parser GenerateSettings
options = GenerateSettings <$> sketchOptions
                           <*> strOption
                             ( long "output"
                            <> short 'o'
                            <> metavar "FILENAME" )
                           <*> argument (return . read)
                             ( metavar "PROGRAM" )
  where programParser str = case readProgram str of
          Left err  -> Nothing
          Right res -> Just res

sketchOptions :: Parser Settings
sketchOptions =
  Settings <$> nullOption
             ( reader (return . map read . words)
            <> long "opcodes"
            <> metavar "OPCODES_LIST" )
           <*> fmap not (switch
             ( help "Do not allow holes to be filled with literal numbers."
            <> long "no-literal-holes" ))
           <*> option
             ( long "holes"
            <> short 'h'
            <> help "Number of holes to generate."
            <> metavar "NUMBER_OF_HOLES" )
           <*> option
             ( long "bits"
            <> short 'b'
            <> help "Size of F18 words in bits."
            <> metavar "BIT_SIZE" )
