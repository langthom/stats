{-|
Module      : CalcStats
Description : Calculate statistics
Copyright   : (c) Thomas Lang, 2014
License     : BSD-3
Stability   : stable
Portability : Imports Data.Typeable and Data.List

This module does all the statistical computations.
So it takes the arguments and the list of numbers
and generates an output String containing all wished
information. The formulas for the results were mostly
taken from the free online encyclopedia Wikipedia.
-}
module CalcStats ( calculate ) where

import Data.List     ( sort   )
import Data.Typeable ( typeOf )


-- |Performs the calculation and prints the result
calculate :: (Eq a, Show a, Num a, Floating a, RealFrac a) => [String] -> [a] -> IO ()
calculate args nums = putStrLn $ "Statistical evaluation: \n\n" ++ buildOutString args nums ""


-- |Builds the ouput String by going through all Parameters
buildOutString :: (Eq a, Show a, Num a, Floating a, RealFrac a) => [String] -> [a] -> String -> String
buildOutString []         _    str = str
buildOutString ("--am":r) nums str = str ++ "Arithmetic Mean: \t\t" ++ show (calcArithM nums) ++ "\n" ++ buildOutString r nums str
buildOutString ("--gm":r) nums str = str ++ "Geometric Mean: \t\t" ++ show (calcGeometricM nums) ++ "\n" ++ buildOutString r nums str
buildOutString ("--hm":r) nums str = if not (any (0==) nums) then str ++ "Harmonic Mean: \t\t\t" ++ show (calcHarmonicM nums) ++ "\n" ++ 
                                                                  buildOutString r nums str
                                                             else str ++ "Harmonic Mean: \t\t\tCannot be computed because of some '0' numbers\n" ++ 
                                                                  buildOutString r nums str
buildOutString ("--me":r) nums str = str ++ "Median: \t\t\t" ++ show (calcMedian nums) ++ "\n" ++ buildOutString r nums str
buildOutString ("--ra":r) nums str = str ++ "Range: \t\t\t\t" ++ show (calcRange nums) ++ "\n" ++ buildOutString r nums str
buildOutString ("--ev":r) nums str = str ++ "Empirical Variance: \t\t" ++ show (calcVariance nums) ++ "\n" ++ buildOutString r nums str
buildOutString ("--es":r) nums str = str ++ "Empirical Standard Deviation: \t" ++ show (calcDeviation nums) ++ "\n" ++ buildOutString r nums str
buildOutString args       nums str = buildOutString (tail args) nums str


-- |Calculates the arithmetic mean of 
-- all numbers in the passed list
calcArithM :: Fractional a => [a] -> a
calcArithM l = (sum l) / fromIntegral (length l)


-- |Calculates the geometrix mean of
-- all numbers in the passed list
calcGeometricM :: Floating a => [a] -> a
calcGeometricM l = (product l) ** (1/(fromIntegral $ length l))


-- |Calculates the harmonic mean of
-- all numbers in the passed list
calcHarmonicM :: (Eq a, Fractional a) => [a] -> a
calcHarmonicM list = (fromIntegral (length list)) / (sum $ map recip list)


-- |Calculates the Median of the passed list, which
-- is the "central" value in the sorted list
calcMedian :: (Ord a, RealFrac a) =>  [a] -> a
calcMedian list = calcQuantil list 0.5


-- |Calculates the Quantil with the passed number
-- of the passed list.
--
-- Note: Indexes - 1 in even part, because Haskell 
-- list indices start at 0
calcQuantil :: (Ord a, RealFrac a, RealFrac b) =>  [a] -> b -> a
calcQuantil list p | not (isInt ((fromIntegral ll) * p)) = l !! ((ceiling $ (fromIntegral ll) * p) - 1)
                   | otherwise                           = ((l !! (pos - 1)) + (l !! pos)) / 2
             where ll = length list
                   l  = sort list
                   pos = truncate $ (fromIntegral ll) * p

                   isInt :: RealFrac a => a -> Bool
                   isInt x = x == fromInteger (round x)


-- |Calculates the range of all numbers in
-- the passed list, which is defined as
-- the difference of the global maximum
-- and the global minimum
calcRange :: (Ord a, Num a) => [a] -> a
calcRange list = maximum list - minimum list


-- |Calculates the corrected variance of the passed list
calcVariance :: (Fractional a) => [a] -> a
calcVariance list = (sum $ map (\x -> (x - am)^2) list) / fromIntegral (length list - 1)
                    where am = calcArithM list


-- |Calculates the standard deviation of the
-- passed list, which is the square root
-- of the corrected variance of the list
calcDeviation :: (Floating a) => [a] -> a
calcDeviation list = sqrt $ calcVariance list

