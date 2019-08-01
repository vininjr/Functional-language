module Automata where

-- Lista de trabalhos praticos 2.

import Control.Monad (foldM)

-- Vamos desenvolver algumas funcoes para simular o comportamento de um automato
-- finito deterministico.

-- Vamos representar os estados de um automato como Ints

type State = Int

-- Cada estado de um automato tem seu conjunto de transicoes.
-- Uma transicao mapeia uma letra do alfabeto para um novo estado. Aqui,
-- assumimos que o alfabeto eh o conjunto de todos os valores validos para o
-- tipo Char.

type Transition = (Char, State)

type StateTransitions = (State, [Transition])

-- Essencialmente, um automato tem:

data Automaton = A { initialState    :: State,             -- um estado inicial;
                     states          :: [State],           -- uma lista de estados;
                     acceptingStates :: [State],           -- os estados de aceitacao;
                     transitions     :: [StateTransitions] -- e as transicoes entre os estados.
                   }

-- Note que, devido a imutabilidade, toda transicao provocara a construcao de um
-- novo automato, e o estado inicial pode, portanto, representar o estado
-- corrente do automato.

-- Vamos escrever algumas funcoes basicas para o funcionamento de um automato:

-- Decidir se um automato estah em um estado de aceitacao.

isAccepting :: Automaton -> Bool
isAccepting aut = (initialState aut) `elem` (acceptingStates aut)

-- Dado uma letra do alfabeto, fazer uma transicao.

-- Vamos tirar proveito do fato de Maybe ser uma monada, e vamos usar a notacao
-- do. Caso uma das <- desempacote um Nothing, a funcao retorna Nothing
transition :: Automaton -> Char -> Maybe Automaton
transition aut l = do currentTransitions <- lookup cs t
                      nextState          <- lookup l currentTransitions
                      return (buildAutomaton nextState)
                   where cs               = initialState aut
                         t                = transitions aut
                         buildAutomaton s = A { initialState    = s,
                                                states          = states aut,
                                                acceptingStates = acceptingStates aut,
                                                transitions     = transitions aut
                                              }

-- Agora que nos sabemos como fazer uma transicao, dado um automato e uma letra
-- do alfabeto, vamos criar uma funcao que faz o automato consumir uma string.
-- Nesse momento, eh natural pensar em usar um foldl para executar as transicoes
-- a partir do automato inicial. O problema eh que a nossa funcao transition nao
-- eh do tipo a -> b -> a, mas sim a -> b -> m a, onde m eh uma monada, nesse
-- caso Maybe. Em Control.Monad, eh definido um foldl que aceita uma funcao
-- retornando um valor dentro de uma monada. Vamos usa-lo

consume :: Automaton -> String -> Maybe Automaton
consume aut str = foldM transition aut str

-- Btw, sempre que tiver um M no fim do nome de uma funcao, ela deve ser o
-- equivalente de uma funcao simples para trabalhar com monadas. Mais ainda,
-- quando houver M_ no final do nome de uma funcao, ela se preocupa apenas com
-- os efeitos provocados por "abrir" a monada, ignorando seu conteudo.

-- Voltando ao desenvolvimento do automato: agora que nos temos uma funcao que 
-- decide se um automato estah em um estado de aceitacao e uma funcao que
-- consome uma string de entrada, podemos escrever a funcao que determina se um
-- automato aceita uma certa string.

-- Vamos usar a funcao maybe para "escapar" da monada Maybe: se alguma transicao
-- falhar, retornamos False.
accept :: Automaton -> String -> Bool
accept aut str = maybe False isAccepting (consume aut str)

-- Agora que nos temos um pouco de codigo para brincar, vamos para as questoes
-- da lista:

-- Questao 1: defina um automato que, dadas strings de '0' e '1' representando
-- naturais, aceita apenas os pares. Faça um que aceita apenas os impares (eh
-- uma mudanca bem simples).

isPar = A { initialState = 1,
			states = [1,2],
			acceptingStates = [1],
			transitions = [(1, [('0', 2),('1', 2)]),
						   (2, [('0', 1),('1', 1)])]
					}
isImpar = A { initialState = 1,
			states = [1,2],
			acceptingStates = [2],
			transitions = [(1, [('0', 2),('1', 2)]),
						   (2, [('0', 1),('1', 1)])]
					}

-- as demais questões estão no outro arquivo Automato.hs
