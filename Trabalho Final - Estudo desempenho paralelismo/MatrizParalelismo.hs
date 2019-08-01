module MatrizParalelismo where

import Control.Parallel
import Control.Parallel.Strategies

type Matriz = [[Int]]



mySumMatr ::  Matriz -> Matriz -> Matriz
mySumMatr [] [] = []
mySumMatr (l:ls) (r:rs) = x `seq` x : y where
                          x = myZipWith (+) l r  
                          y = mySumMatr ls rs
                                   

somaMatrizesDisjoin :: Matriz -> Matriz -> Matriz
somaMatrizesDisjoin xs ys = runEval $ do x <- rpar $ mySumMatr l r
   				         y <- rpar $ mySumMatr ls rs
   			                 return (juntando x y) 
					 where 
					 (l,ls) = disjoin xs
					 (r,rs) = disjoin ys 
					
--parmap (myZipWith (+)) 
myZipWith :: (a-> b ->c) -> [a] -> [b] -> [c]
myZipWith f [] [] = []
myZipWith f [] ys = []
myZipWith f xs [] = []
myZipWith f (x :xs) (y:ys) = r `seq` r : l where
                                r = f x y
                                l = myZipWith f xs ys
--Ira quebrar a Matriz
disjoin :: [a] -> ([a], [a])
disjoin []         = ([], [])
disjoin [x]        = ([x], [])
disjoin (x1:x2:xs) = (x1:ps, x2:qs) where
                     (ps, qs) = disjoin xs
                     
parmap :: (a->b) -> [a] ->Eval [b]
parmap f []     = return []
parmap f (x:xs) = do x'<- rpar $ f x
                     xs'<- parmap f xs
                     return (x':xs')
 
                     
somaMatrizes :: Matriz -> Matriz -> Eval Matriz
somaMatrizes [] [] = return []
somaMatrizes (x:xs) (y:ys) =  do l <- parZipWith (+) x y
                                 r <-  somaMatrizes xs ys
                                 return (l : r)
soma :: Matriz -> Matriz -> Matriz
soma xs ys = runEval $ somaMatrizes xs ys                                
                     
parZipWith :: (a->b->c) -> [a] -> [b] -> Eval [c]
parZipWith f [] [] = return []
parZipWith f [] ys = return []               
parZipWith f xs [] = return []
parZipWith f (x:xs) (y:ys) = do r <- rpar $ f x y
                                l <- parZipWith f xs ys
                                return (r : l)  
                                                   
disjoin2 :: Int -> Matriz  -> (Matriz,Matriz)
disjoin2 0 xs = ([],xs)
disjoin2 n [] = ([],[])
disjoin2 n (x:xs) = ([x] ++ p, r) where (p , r) = disjoin2 (n-1) xs            
					 
--A função "juntando", como o proprio nome ja diz, ira juntar os elementos de suas listas da seguinte
--forma: 1 elemento Lista1 ,1 elemento Lista2, 2 elemento Lista1,2 elemento Lista2  (...)
--dessa forma as linhas da matriz sera organizada, ja que a função disjoin quebra as matriz em elementos de indices
--pares e impares.
juntando ::  [a] -> [a] ->  [a]
juntando [] [] = []
juntando xs [] = xs
juntando [] ys = ys
juntando (x:xs) (y:ys) = [x] ++ [y] ++ (juntando xs ys)
