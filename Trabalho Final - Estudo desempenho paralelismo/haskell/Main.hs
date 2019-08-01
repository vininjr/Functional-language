module Main where

import MatrizParalelismo

main :: IO()
main = program1



program1 :: IO()
program1 = print $ somaMatrizes a b

a :: Matriz
a = newMatriz 100 [1..100000]

b :: Matriz
b = newMatriz 100 [1..100000]

newMatriz :: Int -> [Int] -> Matriz
newMatriz 0 xs = [xs]
newMatriz n xs = xs:newMatriz (n-1) xs
