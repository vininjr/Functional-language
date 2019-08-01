module Aula8 where

import BSTree 
import Aula3 
-- Escreva as funcoes de passeio nas arvores no arquivo BSTree.hs, e as exporte.

preOrderWalk :: BSTree k v -> [v]
preOrderWalk Empty = []
preOrderWalk (Branch key' val' lt rt) = [val'] ++ preOrderWalk lt ++ preOrderWalk rt

inOrderWalk :: BSTree k v -> [v]
inOrderWalk Empty = []
inOrderWalk (Branch key' val' lt rt) = inOrderWalk lt ++ [val'] ++ inOrderWalk rt

postOrderWalk :: BSTree k v -> [v]
postOrderWalk Empty = []
postOrderWalk (Branch key' val' lt rt) = postOrderWalk lt ++ postOrderWalk rt ++ [val']

-- Escreva folds para arvores, tambem no arquivo BSTree.hs, e as exporte

foldlTree :: (a -> b -> a) -> a -> BSTree k b -> a
foldlTree f x tree = foldl f x (inOrderWalk tree) 

foldrTree :: (a -> b -> b) -> b -> BSTree k a -> b
foldrTree f x tree = foldr f x (inOrderWalk tree)

-- Escreva a funcao abaixo, que atualiza o elemento associado a uma chave.
-- Escreva no arquivo BSTree.hs e a exporte.

update :: (Ord k) => BSTree k v -> (k, v) -> BSTree k v
update Empty (k,v) = Empty
update tree@(Branch key val lt rt) (key',val') = decide (myCmp key key') where
                                          decide L = update lt (key',val')
                                          decide E = Branch key' val' lt rt
                                          decide G = update rt (key',val')
                                          

-- Escreva o mergesort. Vc vai precisar escrever as subfuncoes merge e split. Split deve percorrer a lista somente uma vez.
-- primeiraMetade e segundaMetade vao dividir o vetor em 2 partes.
-- Não consegui fazer a split, desculpe mais o tempo foi pouco, fica pra prova...

mySplitMerge :: (Ord a,Num a,Integral a) => [a] -> ([a],[a])
mySplitMerge [] = ([],[])
mySplitMerge l@(x:xs) = myIf (odd x) (x:ps,qs) (ps,x:qs) where
						   (ps,qs) = mySplitMerge xs

mySm :: (Eq a,Num a) => a ->([a],[a]) -> [a]
mySm _ ([],[]) = []
mySm n (xs,ys) = myIf(n==0) (xs) (ys)


primeiraMetade :: [a] -> [a]
primeiraMetade xs = myTake (length xs `div` 2) xs

segundaMetade :: [a] -> [a]
segundaMetade xs = myDrop (length xs `div` 2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys 
merge (x:xs) (y:ys)
		          | (x <= y)  = x:(merge xs (y:ys)) 
		          | otherwise = y:(merge (x:xs) ys)

mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort (primeiraMetade xs)) (mergesort (segundaMetade xs))


-- Retomando o laboratorio anterior...

-- Reescreva myConcat usando foldr.

myConcat :: [[a]] -> [a]
myConcat = undefined

-- Reescreva myTakeWhile usando foldr.

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile = undefined

-- Escreva as funcoes abaixo em termos de foldr

myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

myCycle :: [a] -> [a]
myCycle = undefined
