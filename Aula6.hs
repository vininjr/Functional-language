module Aula6 where

import Aula1 
-- Reescreva myConcat usando foldr.

--myConcat :: [[a]] -> [a]
--myConcat = undefined

-- Reescreva myTakeWhile usando foldr.

--myTakeWhile :: (a -> Bool) -> [a] -> [a]
--myTakeWhile = undefined

--Precisei usar essa funcao, como so tinha ela no caderno, escrevi o codigo aqui, ok?
mySpam :: (a->Bool) -> [a]-> ([a],[a])
mySpam p [] = ([],[])
mySpam p l@(x:xs) = myIf (p x) (x:ps, qs) (([],l)) where
					(ps,qs) = mySpam p xs



-- Escreva a funcao myGroup, que recebe uma lista de elementos e retorna uma lista de listas, onde cada lista eh formada por elementos iguais e consecutivos da lista recebida.
					
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (x:xs) = (x:ys) : myGroup zs where
                 (ys,zs) = mySpam (==x) xs			


-- Agora vamos generalizar myGroup, fornecendo nosso proprio comparador. Como vc definiria myGroup em termos da funcao myGroupBy?

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy _ [] =  []
myGroupBy eq (x:xs) =  (x:ys) : myGroupBy eq zs where
					 (ys,zs) = mySpam (eq x) xs


-- Deduza o que a funcao abaixo deve fazer e a escreva

myFind :: (a -> Bool) -> [a] -> Maybe a
myFind p [] = Nothing
myFind p (x:xs) = myIf(p x) (Just x) (myFind p xs)

-- Escreva as funcoes abaixo em termos de folds
-- So consegui fazer o myAny desta forma, pq não tinha o folds em minhas anotacoes

myAny :: (a -> Bool) -> [a] -> Bool
myAny p [] = False
myAny p (x:xs) = myIf(p x) (True) (myAny p xs)

--myCycle :: [a] -> [a]
--myCycle = undefined
