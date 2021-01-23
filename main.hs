module Main where
    
import System.Environment
import Data.List
import Data.List.Split
import System.Random
import Regex

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("Search", search)
            ,("Generate", generate)
            ,("getRegEx", getRegEx)]
            

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

getRegEx :: [String] -> IO ()
getRegEx [regExp] = do
    let searchExp = parser (lexer regExp)
    putStrLn "The RegEx is:"
    putStrLn (show searchExp)

search :: [String] -> IO ()
search [regExp, fileName] = do
    input <- (readFile fileName)
    let searchExp = parser(lexer regExp)
    let res = get_occurrences (lines(input)) searchExp
    putStrLn "The number of the occurrences are:"
    putStrLn (show res)

generate :: [String] -> IO ()
generate [regExp] = do
    let pattern = parser (lexer regExp)
    generateString pattern 