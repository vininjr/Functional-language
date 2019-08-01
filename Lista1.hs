module Lista1 where

import Control.Applicative
import Control.Monad
import Aula3 (myIf)
import BSTree
import SeqTree


-- Como falamos na ultima aula, ha varios niveis de abstracoes em Haskell,
-- caracterizados por alguns operadores. Eles sao:

-- Operacoes com funcoes simples
-- (sem contexto, altamente previsivel)
-- ($)   :: (a -> b) -> a -> b

-- Operacoes com funcoes simples sobre funtores
-- (contexto determinado pela estrutura de dados f, nao tao previsivel, afinal o conteudo pode variar)
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

-- Operacoes com funcoes contidas em funtores sobre funtores
-- (Contexto influi nao apenas sobre os dados, mas sobre as operacoes, razoavelmente imprevisivel, afinal nao sabemos quantas operacoes serao realizadas)
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- Operacoes monadicas
-- (Caos: o contexto afeta os dados e, pasmem, as operacoes influenciam no contexto)
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b

-- Cada um desses operadores eh regido por uma classe de Haskell.

-- Questao 1
-- Torne a estrutura de dados SeqTree em uma instancia da classe Functor

--ver arquivo SeqTree.hs

-- Questao 2
-- Escreva uma funcao que determina se um numero n pode ser escrito na forma
-- n = 3a + 7b. Em caso afirmativo, a funcao deve retornar a e b.
-- Pode ser feita de forma ineficiente.

decompose37 = (dec37 0 0)

dec37 :: Int -> Int -> Int -> (Int,Int)
dec37 x y 0 = (x,y)
dec37 x y 1 = (x+2,y+1)
dec37 x y 2 = (x+3,y-1)
dec37 x y n = dec37 (x+1) y (n-3) 

-- Questao 3
-- Escreva uma funcao que visita os elementos de uma BSTree em profundidade,
-- retornando-os em uma lista.

buscaProfundidade :: BSTree k v -> [v]
buscaProfundidade = inProfundidade

-- Questao 4
-- Agora, uma funcao que retorna os elementos de uma BSTree em largura.
-- Isto eh, retorna todos os elementos do nivel k, da esquerda para a direita,
-- depois todos os elementos do nivel (k+1), da esquerda para a direita, e assim
-- por diante, comecando com k = 0. Note que existe uma enorme dificuldade em
-- implemenrtar essa ordem usando apenas recursao. Usariamos uma fila, caso
-- dispuséssemos de uma. Será que poderiamos simular uma fila utilizando
-- concatenacao de listas? 



-- Questao 5
-- Escreva uma funcao que retorna a decomposicao em fatores primos de um numero
-- n, retornando seus fatores em uma lista. Pode ser ineficiente.

decomCousins :: Int -> [Int]
decomCousins n = fator n (primos [2..n])
   
fator :: Int -> [Int] -> [Int]
fator 0 xs = []
fator x [] = []
fator n l@(x:xs) = if (n>=x && mod n x == 0)
				   then (x : fator (div n x) l)
				   else (fator n xs)
				                        
primos ::[Int] -> [Int]
primos [] = []
primos (x:xs) = if(divide x x) then (x:primos xs) else (primos xs)					

divide :: Int -> Int -> Bool
divide _ 0 = True
divide x y = if(x==y || y==1) then(divide x (y-1)) 
							  else
				              (if ((mod x y == 0)) then (False) 
				                     else (divide x (y-1)))
				                     
-- Questao 6
-- Escreva uma funcao que, dada uma quantia em centavos, retorna o menor numero
-- possivel de moedas brasileiras para representar essa quantia. Moedas
-- brasileiras tem valores de 5, 10, 25, 50 e 100 centavos. Como eh raro ver
-- moedas de 1 centavo hoje em dia, alguns valores em centavos podem nao ter
-- representacao nas nossas moedas. A sua implementacao pode ser ineficiente.

myMoedas :: Int -> [Int]
myMoedas x = decompor x n where
								n = [100,50,25,10,5]

decompor :: Int -> [Int] -> [Int]
decompor 0 _ = []
decompor x [] = []
decompor y l@(x:xs) = myIf(y>=x) (x : decompor (y-x) l) (decompor y xs)
							


-- Questao 7
-- Implemente a Questao 2 de forma eficiente, usando InfinityTree.
-- Implemente-a de forma eficiente, usando Data.Array.
											 
-- Questao 8
-- Implemente a Questao 5 de forma eficiente, usando InfinityTree
-- Implemente-a de forma eficiente, usando Data.Array.

-- Questao 9
-- Implemente a Questao 6 de forma eficiente, usando InfinityTree
-- Implemente-a de forma eficiente, usando Data.Array.

-- Questao 10
-- Eh notavel que uma lista pode ser usada como uma pilha, dado que ela permite
-- operacoes de insercao e remocao em O(1) apenas na sua extremidade esquerda.
-- Implemente uma fila usando duas listas, cada uma cumprindo o papel de uma
-- pilha.

addFila :: a -> [a] -> [a]
addFila n ls = ls ++ [n]

removeFila :: [a] -> [a]
removeFila [] = []
removeFila (x:xs) = xs
