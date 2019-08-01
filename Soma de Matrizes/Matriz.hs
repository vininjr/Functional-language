module Matriz where

type Matriz = [[Int]]

--Levando em conta que elas tem o mesmo "tamanho".
soma_matriz :: Matriz -> Matriz -> Matriz
soma_matriz [] [] = []
soma_matriz (x:xs) (y:ys) = somaAux x y : soma_matriz xs ys

somaAux :: [Int] -> [Int] -> [Int]
somaAux [] [] = []
somaAux (x:xs) (y:ys) = (x+y) : somaAux xs ys
