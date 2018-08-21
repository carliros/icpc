{-
   Submitting into https://www.spoj.com/problems/PROBTNPO/
   https://uva.onlinejudge.org/index.php?option=com_onlinejudge&Itemid=8&page=show_problem&problem=36

-}
module Main where

main :: IO()
main = interact $ unlines . calcCycles . map read . words

calcCycles :: [Int] -> [String]
calcCycles [] = []
calcCycles (n1:n2:rest)
  = let m    = getMaxCycle n1 n2
        line = (show n1) ++ " " ++ (show n2) ++ " " ++ (show m)
    in line : calcCycles rest

getMaxCycle :: Int -> Int -> Int
getMaxCycle n1 n2 = maximum (map theeNPlusOneLength list)
  where list = if n1 <= n2 then [n1..n2] else [n1, n1-1..n2]


theeNPlusOneLength :: Int -> Int
theeNPlusOneLength 1 = 1
theeNPlusOneLength n
  = 1 + (theeNPlusOneLength m)
  where m = if even n
            then n `div` 2
            else (3 * n) + 1
