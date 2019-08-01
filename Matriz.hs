module Matriz where

type Matriz = [[Int]]

soma_matriz :: Matriz -> Matriz -> Matriz
soma_matriz [] [] = []
soma_matriz xs [] = xs
soma_matriz [] ys = ys
soma_matriz (x:xs) (y:ys) = somaAux x y : soma_matriz xs ys

somaAux :: [Int] -> [Int] -> [Int]
somaAux [] [] = []
somaAux [] ys = ys
somaAux xs [] = xs
somaAux (x:xs) (y:ys) = (x+y) : somaAux xs ys
