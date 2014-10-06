{-
Module      : Plot.hs
Description : Plotting module.
Copyright   : (c) Thomas Lang, 2014
License     : BSD-3
Stability   : stable
Portability : Uses common libraries and LaTex with Tikz.

This module takes a list of numbers and
then produces a LaTex document with
the plot of the function.
-}
module Plot ( plot ) where

import System.IO        ( writeFile, appendFile     )
import System.Directory ( removeFile, doesFileExist )
import Data.List        ( sort, group               )


-- |Main access function
plot :: (Num a, Ord a, Show a) => [a] -> IO ()
plot list = do
  ex <- doesFileExist "./plot.tex"
  case ex of
    False -> procl list
    True  -> removeFile "./plot.tex" >> procl list
               
-- |Further processing, file-writing
procl :: (Num a, Ord a, Show a) => [a] -> IO ()
procl list = do writeFile "./plot.tex" preamble
                let x = group $ sort list
                let y = map length x
                appendFile "./plot.tex" $ 
                  "\\begin{tikzpicture}[x=" ++ 
                  show ((fromIntegral $ length x) / 10) ++ 
                  "cm,y=" ++ show ((fromIntegral $ maximum y) / 10) ++ "cm]\n"
                appendFile "./plot.tex" $ axes x y
                appendFile "./plot.tex" $ graph x
                appendFile "./plot.tex" end


-- |Write graph string
graph :: (Num a, Show a) => [[a]] -> String
graph x = concat $ map (\l@(h:_) -> "\\draw[very thick] (" ++ show h ++ ",0) -- (" ++ show h ++ "," ++ show (length l) ++ ");\n") x


-- |Write Axes String
axes :: (Show a, Num a, Ord a) => [[a]] -> [Int] -> String
axes x y = "\\draw[->] (0,0) -- (" ++ show (length x) ++ ",0);\n \
          \ \\draw[->] (0,0) -- (0," ++ show (maximum y) ++ ");\n\n \
          \ \\foreach \\x in {" ++ show (minimum $ concat x) ++ ",...," ++ show (maximum $ concat x) ++"}\n \
          \  \\draw (\\x,1pt) -- (\\x,-3pt) \n \
          \     node[anchor=north] {\\x}; \n\n \
          \ \\foreach \\y in {0,...," ++ show (maximum y) ++ "}\n \
          \   \\draw (1pt,\\y) -- (-3pt,\\y) \n \
          \      node[anchor=east] {\\y};\n\n"


-- |Write LaTex preamble
preamble :: String
preamble = "\\documentclass[a4paper]{article}\n \
          \ \\usepackage{ tikz } \n \
          \ \\begin{document}\n \n"
                                  
                                  
-- |Write LaTex end-of-file                                 
end :: String
end = "\\end{tikzpicture}\n\\end{document}\n"                                  
                                  
