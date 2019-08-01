module Aula6 where

-- Reescreva myConcat usando foldr.

myConcat :: [[a]] -> [a]
myConcat = undefined

-- Reescreva myTakeWhile usando foldr.

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile = undefined

-- Escreva a funcao myGroup, que recebe uma lista de elementos e retorna uma lista de listas, onde cada lista eh formada por elementos iguais e consecutivos da lista recebida.

myGroup :: Eq a => [a] -> [[a]]
myGroup = undefined

-- Agora vamos generalizar myGroup, fornecendo nosso proprio comparador. Como vc definiria myGroup em termos da funcao myGroupBy?

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy = undefined

-- Deduza o que a funcao abaixo deve fazer e a escreva

myFind :: (a -> Bool) -> [a] -> Maybe a
myFind = undefined

-- Escreva as funcoes abaixo em termos de folds

myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

myCycle :: [a] -> [a]
myCycle = undefined
