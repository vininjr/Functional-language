module Aula2 where

import Aula1 (cc)

myLength :: [a] -> Int
myLength xs = f xs 0 where
	f [] acc = acc
	f (x:xs) acc = f xs (acc + 1)

myConcat :: [[a]] -> [a]
myConcat xss = f xss [] where 
	f [] con = con
	f (x:xs) con = x `cc` f xs con

myReverse :: [a] -> [a]
myReverse xs = f xs [] where
	f [] rev = rev
	f (x:xs) rev = f xs rev `cc` [x]

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProd :: [Int] -> Int
myProd [] = 1
myProd (x:xs) = x * myProd xs

squares ::(Num a) => [a] -> [a]
squares [] = []
squares (x:[]) = [x*x]
squares (x:xs) = [x^2] `cc` squares xs

myIf :: Bool -> a -> a -> a
myIf True x y = x
myIf False x y = y
