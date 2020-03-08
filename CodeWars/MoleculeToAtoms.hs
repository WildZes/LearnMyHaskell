--https://www.codewars.com/kata/52f831fa9d332c6591000511/train/haskell
module MoleculeToAtoms where

import Data.Char

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = Left "Not implemented"

data Token = TokAtom String
           | TokCount Int
           | TokLPar Char
           | TokRPar Char
           deriving (Show,Eq)

tokenizer :: String -> [Token]
tokenizer "" = []
tokenizer (c:c':cs) 
  | elem c ['(','[','{'] = TokLPar c : tokenizer cs
  | elem c [')',']','}'] = TokRPar c : tokenizer cs
  | isAlpha c            = if isUpper c && isAlpha c' && isUpper c' 
                           then TokAtom [c] : tokenizer (c':cs)
                           else let (atom,rest) = span (\x -> isAlpha x && not(isUpper x)) (c':cs)
                                in TokAtom atom : tokenizer rest
--TokAtom (c : takeWhile (not . isUpper) cs) : tokenizer (dropWhile (not . isUpper) cs)
  | isDigit c            = TokCount (read [c]) : tokenizer cs
  | otherwise            = error $ "wrong input" ++ [c]
