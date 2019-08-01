module Aula1 where

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myNull :: [a] -> Bool
myNull [] = True
myNull (x : xs) = False

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x : xs) = x

myTail :: [a] -> [a]
myTail [] = error "empty list"
myTail (x:xs) = xs

myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit [x] = []
myInit (x:xs) = x : myInit xs

cc :: [a] -> [a] -> [a]
cc [] ys = ys
cc (x:xs) ys = x : cc xs ys

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs `cc` myConcat xss

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs `cc` [x]

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (True:xs) = myAnd xs
myAnd (False:xs) = False

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:xs) = True
myOr (False:xs) = myOr xs

myIf :: Bool -> a -> a -> a
myIf True x y = x
myIf False x y = y

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n xs = myIf (n <= 0) ([], xs) (f n ([], xs)) where
                 f 0 (xs, ys) = (xs, ys)
                 f n (xs, (y : ys)) = ((y : ps), qs) where
                                      (ps, qs) = f (n - 1) (xs, ys)
