--Scrabble Score
--https://www.codewars.com/kata/558fa34727c2d274c10000ae/train/haskell

module ScrabbleScore where
import Data.Char (isAlpha, toUpper)

--exercise has preloaded dict with all char values
--I'll try to make dict by my own
-- dict :: [(Char, Int)]
-- Contains only uppercase letters and their score

scrabbleScore :: String -> Int
scrabbleScore = foldr ((+) . getDict . toUpper) 0

getDict :: Char -> Int
getDict ' ' = 0
getDict c   = snd . head $ filter ((c==) . fst) dict

dict :: [(Char,Int)]
dict = [('A',1), ('E',1), ('I',1), ('O',1), ('U',1), ('L',1), ('N',1), ('R',1), ('S',1), ('T',1),
        ('D',2), ('G',2), ('B',3), ('C',3), ('M',3), ('P',3), ('F',4), ('H',4), ('V',4), ('W',4),
        ('Y',4), ('K',5), ('J',8), ('X',8), ('Q',10), ('Z',10)]
