module Aula4 where

-- Talvez vc queira importar algo mais das aulas anteriores.
import Aula3 (myIsPrefixOf)

-- Usando myIsPrefixOf, complete as funcoes abaixo.

myIsInfixOf :: (Eq a) => [a] -> [a] -> Bool
myIsInfixOf = undefined

myIsSuffixOf :: (Eq a) => [a] -> [a] -> Bool
myIsSuffixOf = undefined

-- Sabemos que as funcoes head e tail falham para listas vazias. Vamos escrever versoes seguras delas com a ajuda da monada Maybe

safeHead :: [a] -> Maybe a
safeHead = undefined

safeTail :: [a] -> Maybe [a]
safeTail = undefined

-- Escreva uma funcao que conta quantas vezes um elemento aparece em uma lista

count :: (Eq a) => a -> [a] -> Int
count = undefined

-- Escreva uma função que, dado um elemento, retorna uma lista infinita desse elemento

myRepeat :: a -> [a]
myRepeat = undefined

-- Escreva uma função que, dada uma lista xs, cria uma lista infinita repetindo os elementos de xs

myCycle :: [a] -> [a]
myCycle = undefined
