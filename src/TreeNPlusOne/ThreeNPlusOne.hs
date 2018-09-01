{-
   Submitting into https://www.spoj.com/problems/PROBTNPO/
   https://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=8&page=show_problem&problem=36

-}
module Main where

import Data.Function(fix)

main :: IO()
main = interact $ unlines . calcCycles . map read . words

calcCycles :: [Int] -> [String]
calcCycles [] = []
calcCycles (n1:n2:rest)
  = let m    = getMaxCycle n1 n2
        line = (show n1) ++ " " ++ (show n2) ++ " " ++ (show m)
    in line : calcCycles rest

getMaxCycle :: Int -> Int -> Int
getMaxCycle n1 n2 = maximum (map theeNPlusOneLengthMemo list)
  where list = if n1 <= n2 then [n1..n2] else [n1, n1-1..n2]


memoize :: (Int -> a) -> (Int -> a)
memoize f = (map f [1..] !!)

memo f = (map f [0..] !!)

theeNPlusOneLength :: (Int -> Int) -> Int -> Int
theeNPlusOneLength f 1 = 1
theeNPlusOneLength f 2 = 2
theeNPlusOneLength f n
  | even n = 1 + (f (n `div` 2))
  | odd n  = 1 + (f ((3 * n) + 1))

theeNPlusOneLengthMemo :: Int -> Int
theeNPlusOneLengthMemo = fix (memoize . theeNPlusOneLength)

factFix f n = if n == 0
              then 1
              else (*) n (f (n-1))

fact = fix factFix

factMemo = fix (memo . factFix)

fib f 0 = 0
fib f 1 = 1
fib f n = f(n-1) + f(n-2)

fixMemo = fix (memo . fib)
