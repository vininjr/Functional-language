module Aula2 where

import Aula1 (cc)

-- Vamos escrever as seguintes funcoes usando recursao por cauda:

myLength :: [a] -> Int
myLength xs = f xs 0 where

myConcat :: [[a]] -> [a]
myConcat xss = f xss [] where

myReverse :: [a] -> [a]
myReverse xs = f xs [] where

-- Agora, escreva funcoes que, dadas listas de inteiros, retornam seu somatorio e produtorio, respectivamente:

mySum ::

myProd ::

-- E uma funcao que, dada uma lista de inteiros, retorna a lista com o quadrado desses inteiros:

squares ::

-- Agora, escreva o if:

myIf :: Bool -> a -> a -> a
