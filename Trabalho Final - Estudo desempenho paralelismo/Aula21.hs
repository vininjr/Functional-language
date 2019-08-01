module Aula21 where

import Control.Parallel
import Control.Parallel.Strategies

import Data.List

--main :: IO ()
--main = program2

-- Olar

-- Vcs se lembram da funcao map?
-- Esse eh um jeito interessante de escrever ela.

myMap :: (a -> b) -> [a] -> [b]
myMap f []     = []
myMap f (x:xs) = x':xs' where
                 x'  = f x
                 xs' = myMap f xs

-- Podemos forcar a computacao de x' da seguinte forma

myMap' :: (a -> b) -> [a] -> [b]
myMap' f []     = []
myMap' f (x:xs) = seq x' (x':xs') where
                  x'  = f x
                  xs' = myMap' f xs

sum' :: [Int] -> Int
sum' = foldl' (+) 0

-- A funcao abaixo parece ser cara de se computar.

expensive :: Int -> Int
expensive x = sum' [1..100000000] + x

-- Portanto, esse programa demoraria a executar.

program1 :: IO ()
program1 = print $ myMap' expensive [1..16]
--print :: Show a => a -> IO ()
--(a->b)->a->b

disjoin :: [a] -> ([a], [a])
disjoin []         = ([], [])
disjoin [x]        = ([x], [])
disjoin (x1:x2:xs) = (x1:ps, x2:qs) where
                     (ps, qs) = disjoin xs

-- Leia a questao 3 primeiro.

-- Questao 1
-- Qual seria uma estrategia interessante para executar o programa acima usando
-- dois nucleos? Escreva um program2 que a utilize. Calcule seu speedup.
program2 :: IO ()
program2 = print $ myMap' expensive2 [1..16]

expensive2 :: Int -> Int
expensive2 n = paralelismo [1..10000000]+ n

paralelismo ::[Int] -> Int
paralelismo ls = runEval $ do x <- rpar $ sum' l 
                              y <- rpar $ sum' r
                              return (x + y ) where 
                              (l,r) = disjoin ls 
                           
-- Questao 2
-- Adapte a estrategia desenvolvida na questao 1 para tirar proveito de todos os
-- nucleos do seu computador. Calcule os speedups para k nucleos, onde k varia
-- de 2 ate o numero de nucleos do seu computador.

-- Questao 3
-- Vc conseguiria fazer uma versao generica de map, que paralelizasse a
-- computacao da transformacao de cada elemento? Essa seria uma resposta para
-- as questoes anteriores.
program3 :: IO ()
program3 = print $ myMapPar expensive3 [1..16]
                              
expensive3 :: Int -> Int
expensive3 x = sum' [1..100000000] + x

myMapPar :: (a->b) -> [a] -> [b]
myMapPar f [] = []
myMapPar f (x:xs) = runEval $ do y <- rpar $ f x
                                 return (y : myMapPar f xs ) where 
                                 
                                 seq x' (x':xs')
