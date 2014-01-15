module Sketch where

import Data.Char

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

parse :: String -> [Exp]
parse ""         = []
parse ('$':rest) = VarE (mkName $ takeWhile isAlphaNum rest) :
                   parse (dropWhile isAlphaNum rest)
parse rest       = LitE (StringL $ takeWhile (/= '$') rest) :
                   parse (dropWhile (/= '$') rest)

toExp :: [Exp] -> Exp
toExp = foldr1 $ AppE . AppE (VarE $ mkName "++")

trim :: String -> String
trim = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse

sketch :: QuasiQuoter
sketch = QuasiQuoter { quoteExp = return . toExp . parse . trim }
