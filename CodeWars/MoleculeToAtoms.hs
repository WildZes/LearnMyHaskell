--https://www.codewars.com/kata/52f831fa9d332c6591000511/train/haskell
module MoleculeToAtoms where

import Data.Char

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = Left "Not implemented"

data Token = TokAtom String
           | TokCount Int
           | TokLPar Char
           | TokRPar Char
           | TokEnd
           deriving (Show,Eq)

tokenizer :: String -> [Token]
tokenizer "" = []
tokenizer (c:cs) 
--  | c == '(' = let (expr,ahead) = 
  | elem c ['(','[','{'] = TokLPar c : tokenizer cs
  | elem c [')',']','}'] = TokRPar c : tokenizer cs
  | isAlpha c
    && isUpper c         = if isUpper c && isAlpha (lookAhead cs) && isUpper (lookAhead cs) 
                           then TokAtom [c] : tokenizer cs
                           else let (atom,rest) = span (\x -> isAlpha x && not(isUpper x)) cs
                                in TokAtom (c : atom) : tokenizer rest
--TokAtom (c : takeWhile (not . isUpper) cs) : tokenizer (dropWhile (not . isUpper) cs)
  | isDigit c            = let (count,rest) = span isDigit cs
                           in TokCount (read (c:count)) : tokenizer rest
  | otherwise            = error $ "wrong input" ++ [c]

lookAhead :: String -> Char
lookAhead [] = '\n'
lookAhead (c:cs) = c

toksAhead :: [Token] -> Token
toksAhead [] = TokEnd
toksAhead (t:ts) = t


expression :: [Token] -> [(String,Int)]
expression [] = []
expression toks =
  case toksAhead toks of
       (TokAtom str) -> term toks : expression (tail toks)
       (TokCount _)  -> expression $ tail toks
       (TokLPar par) -> case par of
                             '(' -> let (before,after,num) = parOpen toks ')'
                                    in expression (parClose (before,num)) ++ expression (drop 2 after)
                             '[' -> let (before,after,num) = parOpen toks ']'
                                    in expression (parClose (before,num)) ++ expression (drop 2 after)
                             '{' -> let (before,after,num) = parOpen toks '}'
                                    in expression (parClose (before,num)) ++ expression (drop 2 after)
                             _   -> error "wrong parentheses"
       (TokRPar par) -> error $ "closing bracket should not be here " ++ [par]
       TokEnd        -> expression (tail toks)

getInt :: Token -> Int
getInt (TokCount i) = i
getInt _            = 1

term :: [Token] -> (String,Int)
term [] = error "No token to revert"
term ((TokAtom str):ts) = (str,getInt $ toksAhead ts)

parOpen :: [Token] -> Char -> ([Token],[Token],Int)
parOpen (t:ts) c = let (expr,count) = span (/=TokRPar c) ts
                   in (expr, count, getInt . toksAhead $ drop 1 count)

parClose :: ([Token],Int) -> [Token]
parClose ([],_) = []
parClose ((t:ts),i) = 
  case t of
       (TokAtom x) -> case toksAhead ts of
                           (TokCount _) -> t : parClose (ts,i)
                           _            -> t : TokCount i : parClose (ts,i)
       (TokCount x) -> TokCount (x*i) : parClose (ts,i)
       (TokLPar '(') -> let (before,after,num) = parOpen (t:ts) ')'
                        in parClose (parClose (before,num),i) ++ parClose (drop 2 after,i)
       (TokRPar ')') -> error "Incorrect closing"
--       (TokRPar ')') -> parClose (ts,i)
       (TokLPar '[') -> let (before,after,num) = parOpen (t:ts) ']'
                        in parClose (parClose (before,num),i) ++ parClose (drop 2 after,i)
       (TokRPar ']') -> error "Incorrect closing"
--       (TokRPar ']') -> parClose (ts,i)
       (TokLPar '{') -> let (before,after,num) = parOpen (t:ts) '}'
                        in parClose (parClose (before,num),i) ++ parClose (drop 2 after,i)
       (TokRPar '}') -> error "Incorrect closing"
--       (TokRPar '}') -> parClose (ts,i)
       _            -> error "Incorrect closing"

getSum :: [(String,Int)] -> (String,Int)
getSum tls@((x,y):xys) = (x,getY)
  where getY = sum . (map snd) . (filter ((==x) . fst)) $ tls

removeItem :: (String,Int) -> [(String,Int)] -> [(String,Int)]
removeItem (x,_) tls = filter (\a -> fst a /= x) tls

getAns :: [(String,Int)] -> [(String,Int)]
getAns [] = []
getAns tls@((x,y):xys) = getSum tls : getAns (removeItem (x,y) xys)

