module MatrizParalelismo where

import Control.Parallel
import Control.Parallel.Strategies

type Matriz = [[Int]]

--main() :: IO
--Custos:
-- >SomaMatrizes : o custo sera o tempo para pecorrer a lista uma vez e meia (  2(n + n/2)), onde n é o tamanho da lista
-- >SomaMatrizesRecursiva : o custo POSSIVELMENTE será o tempo de para pecorrer a 
--lista uma vez mais o tempo que a linguagem joga o processamento para os nucleos (TN),assim n + TN
-- >  somaMatrizesDisjoin : o custo sera o tempo de quebra das listas(d) + o tempo para somar as linhas (sL) + o tempo para juntar o resultado da soma

somaMatrizes :: Matriz -> Matriz -> Matriz
somaMatrizes xs ys = runEval $ do x <- rpar $ mySumMatr l r
   				  y <- rpar $ mySumMatr ls rs
				  return (x ++ y) 
				  where 
				  (l,ls) = mySplitAt (div (length xs) 2) xs
                                  (r,rs) = mySplitAt (div (length ys) 2) ys 
							      
mySplitAt :: Int -> Matriz -> (Matriz,Matriz)
mySplitAt n [] = ([],[])
mySplitAt n (x:xs) = if (n >= 0) then (x : l, ls) else ([x] , xs) where 
                                                  (l,ls) = mySplitAt (n-1) xs

mySumMatr ::  Matriz -> Matriz -> Matriz
mySumMatr [] [] = []
mySumMatr (l:ls) (r:rs) =  somaAux l r : mySumMatr ls rs where
				somaAux [] [] = []
				somaAux [] ys = ys
				somaAux xs [] = xs
				somaAux (x:xs) (y:ys) = (x+y) : somaAux xs ys

-------------------------------------------------------------------------

--Testa essa forma
--a ideia é usar a recurção para pecorrer somente uma vez a lista
-- estou tentando usar paralelismo gererico, 
--Logica: faço a soma das 'linhas' da matriz e chamo a funcao recursivamente
--para o restante da matriz. Quando for feita a soma de linhas, o haskell 'jogara' esse processo para um nucleo.
-- De uma forma resumida o haskell ira espalhar entre os nucleos disponiveis na maquina, a soma de linhas (somaLista x y) que ocorrerá
-- durante a recuursão

--somaMatrizesRecursao:: Matriz -> Matriz -> Matriz 
--somaMatrizesRecursao [] [] = []
--somaMatrizesRecursao (x :xs) (y:ys) = runEval $ do x' <- rpar $ somaLista x y
  --                                                 y' <- rpar $ somaMatrizesRecursao xs ys
	--							      return (x' : y')


somaLista :: [Int] -> [Int] -> [Int]
somaLista [] [] = []
somaLista [] ys = ys
somaLista xs [] = xs
somaLista (x:xs) (y:ys) = (x+y) : somaLista xs ys

--testarRecursao :: Matriz -> Matriz -> Matriz
--testarRecursao xs ys = runEval $ do x <- somaMatrizesRecursao xs ys 
                                    --return (x)
-------------------------------------------------------------------------
--Testar 
--somaMatrizesDisjoin :: Matriz -> Matriz -> Matriz
--somaMatrizesDisjoin xs ys = runEval $ do x <- rpar $ mySumMatr l r
  -- 				                         y <- rpar $ mySumMatr ls rs
   	--		                             return (jun) 
		--								 where 
			--							 (l,ls) = disjoin xs
				--						 (r,rs) = disjoin ys 
					--					 jun = juntando x y


--Ira quebrar a Matriz
disjoin :: [a] -> ([a], [a])
disjoin []         = ([], [])
disjoin [x]        = ([x], [])
disjoin (x1:x2:xs) = (x1:ps, x2:qs) where
                     (ps, qs) = disjoin xs
					 
--A função "juntando", como o proprio nome ja diz, ira juntar os elementos de suas listas da seguinte
--forma: 1 elemento Lista1 ,1 elemento Lista2, 2 elemento Lista1,2 elemento Lista2  (...)
--dessa forma as linhas da matriz sera organizada, ja que a função disjoin quebra as matriz em elementos de indices
--pares e impares.
juntando :: [a] -> [a] -> [a]
juntando [] [] = []
juntando xs [] = xs
juntando [] ys = ys
juntando (x:xs) (y:ys) = [x] ++ [y] ++ (juntando xs ys)
