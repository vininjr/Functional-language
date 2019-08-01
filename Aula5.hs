module Aula5 where

import Aula3 (myIf, myOr)

myZip :: [a] -> [b] -> [(a, b)]
myZip = myZipWith (,)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) = myIf (p x) (x : myTakeWhile p xs) ([])

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p l@(x:xs) = myIf (p x) (myDropWhile p xs) (l)

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan p [] = ([], [])
mySpan p l@(x:xs) = myIf (p x) (x:ps, qs) ([], l) where
                    (ps, qs) = mySpan p xs

myNot :: Bool -> Bool
myNot True = False
myNot False = True

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

myBreak :: (a -> Bool) -> [a] -> ([a], [a])
myBreak p xs = mySpan (myNot `comp` p) xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x:xs) = myIf (p x) (x : myFilter p xs) (myFilter p xs)

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] ys = []
myZipWith f xs [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f e [] = e
myFoldl f e (x:xs) = myFoldl f (f e x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f e [] = e
myFoldr f e (x:xs) = x `f` (myFoldr f e xs)
