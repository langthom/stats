{-|
Module      : Parsing
Description : Parsing the arguments
Copyright   : (c) Thomas Lang, 2014
License     : BSD-3
Stability   : stable
Portability : Imports module "CalcStats" and Text.ParserCombinators.Parsec

This module parses the command line arguments.
As implemented here, if only the filename is
passed (no other arguments), all statistical 
computations will be performed.
It also reacts if parameters are not legal or
too few/many arguments were passed. Furthermore
it holds functions for printing a help and a 
version message.
-}
module Parsing ( parseArgs ) where

import Text.ParserCombinators.Parsec

import CalcStats


------------------------ [ARGUMENT PARSING SECTION] ----------------------

-- |Checks if correct, too much or too few parameters
-- Special check for "--help" Flag. If this is passed,
-- the help message will be displayed and NO FURTHER PROCESSING
parseArgs :: [String] -> String -> IO ()
parseArgs args fname | "--help" `elem` args           = printHelp
                     | "--version" `elem` args        = printVersion
                     | length args < 1                = error "Too few parameters."
                     | length args > 8                = error "Too many parameters."
                     | not (correctArgs $ init args)  = error "Incorrect parameter(s), use --help for more information."
                     | otherwise                      = procArgs (init args) fname


-- |Helper function do determine, if the
-- passed arguments are valid
correctArgs :: [String] -> Bool
correctArgs x = all correct x || x == []
    where correct x = x `elem` params


-- |List of all available parameters
params :: [String]
params = ["--help","--version","--am","--gm","--hm","--me","--ra","--ev","--es"]


-- |Parses the passed CSV file, removes all spaces (because they 
-- raise Errors during conversion), converts the Strings to Numbers
-- and calls the calculating function
procArgs :: [String] -> String -> IO ()
procArgs args fname = do
                         file <- readFile fname
                         case parseFile file of
                           Left err   -> putStrLn $ show err
                           Right list -> let l =  map (\x -> read x :: Double) (removeSpaces $ concat list)
                                         in case args of 
                                           [] ->  calculate params l
                                           _  ->  calculate args   l

-- |helper function for removing all Spaces and empty Strings in the list  
removeSpaces = filter (\x -> (not (" " == x)) && (not ("" == x)))


------------------- [FILE PARSING SECTION] ----------------

-- Notes:
--
-- Here a CSV file has lines, that are separated by eol.
-- Each line has cells, that are comma separated.
--
-- Thanks to the authors of the book "Real World Haskell"
-- who created this minimalistic CSV parser.
--                           
-- Internal Notes:
--
-- noneOf -> succeeds if the one of the passed character                          
--           are NOT the read ones, returns the parsed char
-- many   -> applies the passed parser 0 or more times

-- |Parses the text stored in "file"
parseFile :: String -> Either ParseError [[String]]
parseFile file = parse csvFile "(unknown)" file

-- |Definitions of the structure of a valid CSV file
csvFile = endBy line eol
line    = sepBy cell (char ',')
cell    = many (noneOf ",\n\r")
eol     =  try (string "\n\r")
       <|> try (string "\r\n")
       <|> string "\n"
       <|> string "\r"


-------------------- [HELP SECTION] ------------------------

-- |Prints the help message on the screen
printHelp :: IO ()
printHelp = putStrLn helpMsg 

-- |The help message
helpMsg :: String
helpMsg = "usage: stats [OPTIONS] FILENAME\n\n\
       \ available Options:\n \
       \ --help\t\tPrints this message\n \
       \ --version\t\tPrints out the version\n \
       \ --am\t\t\tArithmetic Mean \n \
       \ --gm\t\t\tGeometric Mean \n \
       \ --hm\t\t\tHarmonic Mean \n \
       \ --me\t\t\tMedian \n \
       \ --ra\t\t\tRange \n \
       \ --ev\t\t\tEmpirical Standard Variance\n \
       \ --es\t\t\tEmpirical Standard deviation\n"
         
-- |Prints the version message on the screen         
printVersion :: IO ()
printVersion = putStrLn versionMsg         

-- |The version message
versionMsg :: String
versionMsg = "Stats - The Statistical Command Line Tool written in Haskell only.\n \
      \ Author : Thomas Lang\n \
      \ Version: 1.0  2014/10/02\n"