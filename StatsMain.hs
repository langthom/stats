{-|
Module      : Main
Description : Main module for Stats.
Copyright   : (c) Thomas Lang, 2014
License     : BSD-3
Stability   : stable
Portability : Imports module "Parsing".

This is the main module of Stats, the free
command line statistical software.
-}
module Main ( main ) where

import System.Environment ( getArgs )
import Data.List          ( last )

import Parsing


-- |Main Function, gets command line arguments
-- and call the parsing function
--
-- Note: Here the filename of the CSV file
--       has to be the last Parameter
main :: IO ()
main = do
  args <- getArgs
  parseArgs args (last args)
    

