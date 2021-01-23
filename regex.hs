module Regex (parser, lexer, get_occurrences, generateString) where

import System.Environment
import Data.List
import Data.List.Split
import System.Random

data Characters = Bound Int | Specified Char deriving (Show, Eq)

data Regexp = Any 
            | StartsWith [Regexp]
            | EndsWith [Regexp]
            | Lit Char 
            | Many Regexp 
            | OneOr Regexp 
            | Exactly (Regexp) Int 
            | Or Regexp Regexp 
            | Set [Characters] 
            deriving (Show, Eq)
 
data BOps = ExactlyOp | OrOp deriving Show
data UOps = StartsWithOp | EndsWithOp | ManyOp | OneOrOp deriving Show

data Token = Sym Char | ISym Int 
    | BOp BOps | UOp UOps | All | To
    | LPar | RPar | LBracket | RBracket | LBrace | RBrace
    | RE Regexp
    | Err
  deriving Show

lexer :: String -> [Token]
lexer s = map classify (init (splitOn "," (addCommas s)))


addCommas :: String -> String
addCommas "" = ""
addCommas (' ' : s) = " ," ++ addCommas s
addCommas ('(' : s) = "(," ++ addCommas s
addCommas (')' : s) = ")," ++ addCommas s
addCommas ('{' : s) = "{," ++ addCommas s
addCommas ('}' : s) = "}," ++ addCommas s
addCommas ('[' : s) = "[," ++ addCommas s
addCommas ('.' : s) = ".," ++ addCommas s
addCommas ('-' : s) = "-," ++ addCommas s
addCommas ('^' : s) = "^," ++ addCommas s
addCommas ('$' : s) = "$," ++ addCommas s
addCommas ('*' : s) = "*," ++ addCommas s
addCommas ('+' : s) = "+," ++ addCommas s
addCommas ('|' : s) = "|," ++ addCommas s
addCommas (x : s) = x : "," ++ addCommas s


classify :: String -> Token
classify "(" = LPar
classify ")" = RPar
classify "{" = LBrace
classify "}" = RBrace
classify "[" = LBracket
classify "]" = RBracket
classify "." = All
classify "-" = To
classify "^" = UOp StartsWithOp
classify "$" = UOp EndsWithOp
classify "*" = UOp ManyOp
classify "+" = UOp OneOrOp
classify "|" = BOp OrOp 
classify " " = Sym ' '
classify (x : s) | isAlpha x = Sym x
classify (x : s) | isDigit x = ISym (read (x : ""))
classify _ = Err

parser :: [Token] -> [Regexp]
parser l = sr [] l

sr :: [Token] -> [Token] -> [Regexp]
sr (Sym x : ts) i = sr (RE (Lit x) : ts) i
sr (All : ts) i = sr (RE (Any) : ts) i
sr (UOp ManyOp : RE r : ts) i = sr (RE (Many r) : ts) i
sr (UOp OneOrOp : RE r : ts) i = sr (RE (OneOr r) : ts) i
sr (RBrace : ISym n : LBrace : RE r : ts) i = sr (RE (Exactly r n) : ts) i
sr (RE r : BOp OrOp : RE r1 : ts) i = sr (RE (Or r1 r) : ts) i
sr (RE r : UOp StartsWithOp : ts) i = sr (RE (StartsWith [r]) : ts) i
sr (RE r : RE (StartsWith l) : ts) i = sr (RE (StartsWith (l ++ [r])) : ts) i
sr (UOp EndsWithOp : ts) i = [EndsWith (toList ts)]
sr (RBracket : ISym n : To : ISym n1 : LBracket : ts) i = sr (RE (Set (Bound n1 : Specified '-' : [Bound n])) : ts) i
sr (RBracket : RE (Lit x) : To : RE (Lit x1) : LBracket : ts) i = sr (RE (Set (Specified x1 : Specified '-' : [Specified x])) : ts) i
sr (RE (Lit x) : RE (Lit x1) : LBracket : ts) i = sr (RE (Set (Specified x1 : [Specified x])) : LBracket : ts) i
sr (RBracket : RE (Lit x) : RE (Set r) : LBracket : ts) i = sr (RE (Set (r ++ [Specified x])) : ts) i
sr xs (i:is) = sr (i:xs) is
sr reg [] = toList reg
--sr reg [] = error (show reg)

toList :: [Token] -> [Regexp]
toList [] = []
toList (RE r : xs) = (toList xs) ++ [r]

generateString :: [Regexp] -> IO ()
generateString [] =  do
    putStrLn ""
generateString (Any : re) = do
    g <- newStdGen
    let i = take 1 (randomRs ('0', '9') g)
    if i >= "5" then
        let c = take 1 (randomRs ('a', 'z') g)
        in putStr c
    else
        putStr i
    generateString re 
generateString (StartsWith [] : re) = do
    generateString re
generateString (StartsWith (x:xs) : re) = do
    generateString [x]  
    generateString (StartsWith (xs) : re) 
generateString (Lit x : re) = do
    putChar x 
    generateString re
generateString (Many (Lit x) : re) = do
    g <- newStdGen
    let i = take 1 (randomRs ('0', '9') g)
    putStr (take (read (i)) (repeat x)) 
    generateString re 
generateString (OneOr (Lit x) : re) = do
    putChar x
    g <- newStdGen
    let i = take 1 (randomRs ('0', '9') g)
    putStr (take (read (i)) (repeat x)) 
    generateString re 
generateString (Exactly (Lit x) i : re) = do
    putStr (take i (repeat x))
    generateString re 
generateString (Or r r1 : re) = do 
    g <- newStdGen
    let i = take 1 (randomRs ('0', '9') g)
    if i >= "5" then
        generateString (r : re) 
    else
        generateString (r1 : re)
generateString (Set (Bound x : Specified '-' : Bound x1 : xs) : re) = do
    g <- newStdGen
    let i = take 1 (randomRs (x, x1) g)
    putStr ((show (i !! 0))) 
    generateString re 
generateString (Set (Specified x : Specified '-' : Specified x1 : xs) : re) = do
    g <- newStdGen
    let i = take 1 (randomRs (x, x1) g)
    putStr i 
    generateString re 
generateString (Set (Specified x : xs) : re) = do
    g <- newStdGen
    let i = take 1 (randomRs ('0', '9') g)
    if i >= "5" then do
        putChar x 
        generateString re
    else generateString (Set (xs) : re)

get_occurrences :: [String] -> [Regexp] -> Int
get_occurrences [] r = 0
get_occurrences (x:xs) r = (exec x r []) + get_occurrences xs r 

exec :: String -> [Regexp] -> [Regexp] -> Int
exec [] (EndsWith [] : re) state = 1
exec input@(s:ss) (EndsWith [] : re) state = exec input state []
exec [] (r:re) state = 0
exec [] [] state = 1
exec (s:ss) [] state = 1 + (exec (s:ss) state [])
exec input@(s:ss) reg@(Lit x : re) state | match input (Lit x) = exec ss re (state ++ [Lit x])
    | otherwise = exec ss (state ++ reg) []
exec input@(s:ss) reg@(Set x : re) state | match input (Set x) = exec ss re (state ++ [Set x])
    | otherwise = exec ss (state ++ reg) []
exec input@(s:ss) reg@(Any : re) state  = exec ss re (state ++ [Any])
exec input@(s:ss) reg@(Exactly (r) n : re) state | match input (Exactly (r) n) = exec (drop n input) re (state ++ [(Exactly (r) n)])
    | otherwise = exec ss (state ++ reg) []
exec input@(s:ss) reg@(Many x : re) state | match input (Many x) = exec ss reg state
    | otherwise = exec input re (state ++ [Many x])
exec input@(s:ss) reg@(OneOr x : re) state@(OneOr x1 : st) | x == x1 && not (match input (OneOr x)) = exec input re (st ++ [OneOr x])
    | otherwise = exec ss reg state
exec input@(s:ss) reg@(OneOr x : re) state | match input (OneOr x) = exec ss reg ((OneOr x) : state)
    | otherwise = exec input (state ++ reg) []
exec input@(s:ss) reg@(Or r r1 : re) state | match input r || match input r1 = exec ss re (state ++ [Or r r1])
    | otherwise = exec ss (state ++ reg) []
exec input@(s:ss) reg@(StartsWith [] : re) state = exec input re [StartsWith state] 
exec input@(s:ss) reg@(StartsWith (x:xs) : re) state | match input x = exec ss (StartsWith (xs) : re) (state ++ [x])
    | otherwise = 0
exec input@(s:ss) reg@(EndsWith (x:xs) : re ) state | match input x = exec ss (EndsWith (xs) : re) (state ++ [x])
    | otherwise = exec ss (state ++ reg) []
--exec s r state = error (show state)


match :: String -> Regexp -> Bool
match (s:ss) (Lit x) = s == x
match s (Any) = True 
match (s:ss) (Set (Bound x : Specified '-' : Bound x1 : xs)) | isDigit s = (read (s : "")) > x && (read (s : "")) < x1
    | otherwise = False
match (s:ss) (Set (Specified x : Specified '-' : Specified x1 : xs)) | isAlpha s = s > x && s < x1
    | otherwise = False
match all@(s:ss) (Set (Specified x : xs)) = s == x || match all (Set(xs))
match s (Set ([])) = False
match all@(s:ss) (Exactly (r) n) | match all r && n > 0 = match ss (Exactly (r) (n-1))
    | n == 0 = True
    | otherwise = False
match all@(s:ss) (Many r) = match all r 
match all@(s:ss) (OneOr r) = match all r
match s r = error (show s ++ "match")

isAlpha :: Char -> Bool
isAlpha x = ('A' <= x && x <= 'Z') || ('a' <= x && x <= 'z')

isDigit :: Char -> Bool
isDigit x = '0' <= x && x <= '9'

