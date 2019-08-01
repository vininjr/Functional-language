module Gabarito where

myIf :: Bool -> a -> a -> a
myIf True x y  = x
myIf False x y = y

-- Questao 1

insert :: Ord a => a -> [a] -> [a]
insert x []       = [x]
insert x l@(y:ys) = myIf (x > y) (y : insert x ys) (x:l)

insertionSort :: Ord a => [a] -> [a]
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Questao 2

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] ys           = True
isSubsequenceOf xs []           = False
isSubsequenceOf l@(x:xs) (y:ys) = myIf (x == y) (isSubsequenceOf xs ys)
                                                (isSubsequenceOf l ys)

-- Questao 3

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt 0 xs     = ([], xs)
mySplitAt n []     = ([], [])
mySplitAt n (x:xs) = (x:ps, qs) where
                   (ps, qs) = splitAt (n - 1) xs

-- Questao 4

myId :: [a] -> [a]
myId = foldr (:) []

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
