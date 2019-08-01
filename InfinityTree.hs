module InfinityTree where

import Aula3 (myIf)
import Lista1 (decomCousins)
data InfinityTree a = Branch a (InfinityTree a) (InfinityTree a)

repeatTree :: a -> InfinityTree a
repeatTree x = Branch x (repeatTree x) (repeatTree x)

instance Functor InfinityTree where
        fmap f (Branch x lt rt) = Branch (f x) (fmap f lt) (fmap f rt)

instance Applicative InfinityTree where
        pure x = repeatTree x
        (Branch f lt1 rt1) <*> (Branch x lt2 rt2) = Branch (f x) (lt1 <*> lt2)
                                                                 (rt1 <*> rt2)

toList :: InfinityTree a -> [a]
toList (Branch x lt rt) = x : alternate ls rs where
                          alternate [] []         = []
                          alternate xs []         = xs
                          alternate [] ys         = ys
                          alternate (x:xs) (y:ys) = x:y:alternate xs ys
                          ls = toList lt
                          rs = toList rt

naturalsTree :: InfinityTree Int
naturalsTree = Branch 0 oddsTree evensTree where
               oddsTree  = fmap ((+1) . (*2)) naturalsTree
               evensTree = fmap ((+2) . (*2)) naturalsTree

search :: InfinityTree a -> Int -> a
search (Branch x lt rt) 0 = x
search (Branch x lt rt) n = if   (r == 0)
                            then search rt (q - 1)
                            else search lt q where
                            (q, r) = n `divMod` 2

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

memofib :: Int -> Int
memofib = search fibsTree where
          fibsTree = f <$> naturalsTree
          f 0 = 0
          f 1 = 1
          f n = memofib (n - 1) + memofib (n - 2)
      
    --decompor 3 e 7
          
decomp :: Int -> (Int,Int)
decomp = search decomTree where
		 decomTree = dec <$> naturalsTree

dec n = dec37 n 0 0

dec37 :: Int -> Int -> Int -> (Int,Int)
dec37 0 x y = (x,y)
dec37 1 x y = (x+2,y+1)
dec37 2 x y = (x+3,y-1)
dec37 n x y = dec37 (n-3) (x+1) y

	--fatores primos

fatorPrimo :: Int -> [Int]
fatorPrimo = search fatorTree where
			 fatorTree = decomCousins <$> naturalsTree
			 
			 
