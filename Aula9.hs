module Aula9 where


import Aula3 (myIf)
-- Escreva a funcao enumerate.

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate list = zip count list where count = [1..]
								

-- Foldr

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat xss = foldr (++) [] xss

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p =  foldr (\x xs ->myIf(p x)(x:xs) ([])) []

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\x xs ->myIf (p x) (True) (xs)) False

myCycle :: [a] -> [a]
myCycle [] = []
myCycle xs = foldr (:) [] (xs ++ myCycle xs) 

-- Foldl

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum xs = foldl (+) 0 xs

myProd :: (Num a) => [a] -> a
myprod [] = 0
myProd xs = foldl (*) 1 xs

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = foldl (*) 1 [1..n]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = foldl (flip (:)) [] xs  
