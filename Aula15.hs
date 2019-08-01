module Aula15 where

import qualified Data.Map.Strict as Map

import Aula3 (myIf)
import Aula14 (countConsecutives, countInitWords)

-- A notacao do funciona essencialmente com qualquer monada, nao apenas IO.
-- Veja, por exemplo, que podemos definir map de uma forma meio estranha
-- usando essa notacao.

-- IO eh a monada que abstrai efeitos de entrada e saida no programa.
-- Maybe eh a monada que abstrai a chance de falha em uma funcao.
-- Listas sao monadas que abstraem o nao-determinismo.

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = do x <- xs
                return (f x)

-- Implemente a funcao filter, usando a notacao do:
-- Dica: sempre que quiser retornar algum elemento, use return; sempre que for
-- preciso retornar nada, use a lista vazia.

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = do x <- xs
                   myIf (p x)
                        (return x)
                        []

-- Agora, usando as funcoes da Aula14, nos devemos estar aptos a construir uma
-- cadeia de Markov.

-- Primeiro, vai ser preciso usar Doubles em vez de Ints, jah que precisaremos
-- guardar resultados fracionários de uma certa divisao. Basta trocar o tipo nas
-- funcoes de Aula14.

-- Escreva uma funcao que, dada uma string de entrada, toma o mapeamento
-- produzido por countConsecutives e, para cada chave (str1, str2) desse
-- mapeamento, divide seu valor associado pelo numero de ocorrencias de str1 na
-- string de entrada (use countWords). A funcao mapWithKey pode ser util.

computeChance :: String -> Map.Map (String, String) Double
computeChance str = Map.mapWithKey f (countConsecutives str) where
                    f (w1, w2) c = c / (maybe 0 id (Map.lookup w1 mapwords))
                    mapwords     = countInitWords str

-- Entao pessoal, eu nao me atentei para o fato de que, para fazer essa divisao,
-- a gente nao pode colocar a ultima palavra do texto na contagem, coisa que
-- countWords faz. Para resolver isso, criei a funcao countInitWords, que apenas
-- desconsidera a ultima. Como eu tambem nao havia me atentado para esse detalhe
-- antes de resolver a lista, eu considerarei quem tiver contado a ultima
-- palavra na divisao.
