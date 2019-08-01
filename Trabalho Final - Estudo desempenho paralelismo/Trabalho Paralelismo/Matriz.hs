module Matriz where

import Control.Parallel
import Control.Parallel.Strategies

type Matriz = [[Int]]

soma :: Matriz -> Matriz -> Matriz
soma xs ys = runEval $ somaMatrizes xs ys                                
  
somaMatrizes :: Matriz -> Matriz -> Eval Matriz
somaMatrizes [] [] = return []
somaMatrizes (x:xs) (y:ys) =  do l <- parZipWith (+) x y
                                 r <-  somaMatrizes xs ys
                                 return (l : r)
                
parZipWith :: (a->b->c) -> [a] -> [b] -> Eval [c]
parZipWith f [] [] = return []
parZipWith f [] ys = return []               
parZipWith f xs [] = return []
parZipWith f (x:xs) (y:ys) = do r <- rpar $ f x y
                                l <- parZipWith f xs ys
                                return (r : l)  
                                
--n1 = 8,155 , C 10000 L 1000
--n2 = 6,549 , C 10000 L 1000

--n1 = 13,014, C 1000 L 10000 
--n2 = 10,596, C 1000 L 10000

--n1 = 0,832 , C 1000 L 1000
--n2 = 0,695 , C 1000 L 1000

