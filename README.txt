 
               STATS - The command line statistics tool
               ----------------------------------------
               Author: Thomas Lang
               (c) Thomas Lang, 2014


What can it do?
---------------
STATS can perform basic statistical computations on a
csv file. For your own testing, I provide a Test.csv 
file, which contains 2,000 Integers.


What do I need?
---------------
To build and use this application, you have to have installed
the Haskell programming language, the Glasgow Haskell Compiler
(abbreviated as GHC) and the Parsec package.
Under Linux you can easily install this via:

(Ubuntu/Debian version)

$> apt-get update
$> sudo apt-get install haskell-platform
$> cabal install cabal-install
$> cabal update
$> cabal install parsec


How to build it?
----------------
There are two several ways of installing this program:
In order to build it locally, just clone this repository, change
into the directory and type "make".
If you want to globally install it on your system, type
"make install" instead.

An additional form is to create a zipped tarball via "make package".


How do I use it?
----------------
To use it, call it in the following way:

$>  stats  [OPTIONS]  file.csv

Available options:

o --help
  Displays a help message

o --version
  Displays a version message

o --am
  Calculate the arithmetic mean
  See http://en.wikipedia.org/wiki/Arithmetic_mean

o --gm
  Calculate the geometric mean
  See http://en.wikipedia.org/wiki/Geometric_mean

o --hm
  Calculate the harmonic mean
  See http://en.wikipedia.org/wiki/Harmonic_mean

o --me
  Calculate the Median, which is the 0.5 quantile
  See http://en.wikipedia.org/wiki/Median

o --ra
  Calculate the range, which is the difference between
  the global maximum and the global minimum of all values
  See http://en.wikipedia.org/wiki/Range_%28statistics%29

o --ev
  Calculate the sample covariance
  See http://en.wikipedia.org/wiki/Sample_mean_and_sample_covariance

o --es
  Calculate the standard deviation
  See http://en.wikipedia.org/wiki/Standard_deviation

o --plot
  Plots the numbers in a typcial histogram.
  This option will generate the necessary LaTex output file.
  To view it, you will have to have installed the Tikz libraries.
  If so, then you can compile it via

  $> pdflatex plot.tex

  Next you can view it with your favourite PDF viewer.


Note about usage:
If you call it with one or more of the parameters listed
above, the program will only compute these individual values.
If you call it with only the file name, all available options
will be computed (without help, version or plot of course).


