module Main where

import Matriz

main :: IO()
main = program1

program1 :: IO()
program1 = print $ soma a b

a :: Matriz
a = newMatriz 10000 [1..5000]

b :: Matriz
b = newMatriz 10000 [1..5000]

newMatriz :: Int -> [Int] -> Matriz
newMatriz 0 xs = [xs]
newMatriz n xs = xs : newMatriz (n-1) xs
