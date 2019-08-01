module Main where

import Matriz
import Control.DeepSeq

import Control.Parallel
import Control.Parallel.Strategies
main ::IO()
main = program1

program1 :: IO()
program1 = print $ mySum a b c d

mySum :: Matriz -> Matriz -> Matriz -> Matriz -> Matriz
mySum a b c d = soma_matriz (soma_matriz a b) (soma_matriz c d)

a :: Matriz
a = [[1..1000],[1..1000],[1..1000],[1..1000]]

b :: Matriz
b = [[1..1000],[1..1000],[1..1000],[1..1000]]

c :: Matriz
c = [[1..1000],[1..1000],[1..1000],[1..1000]]

d :: Matriz
d = [[1..1000],[1..1000],[1..1000],[1..1000]]
