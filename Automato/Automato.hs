module Automato where

-- Esse é o automato modificado para maps    

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map

-- Vamos desenvolver algumas funcoes para simular o comportamento de um automato
-- finito deterministico.

-- Vamos representar os estados de um automato como Ints

type State = Int

-- Cada estado de um automato tem seu conjunto de transicoes.
-- Uma transicao mapeia uma letra do alfabeto para um novo estado. Aqui,
-- assumimos que o alfabeto eh o conjunto de todos os valores validos para o
-- tipo Char.

type Transition = Map.Map Char State

type StateTransitions = Map.Map State Transition

-- Essencialmente, um automato tem:

data Automaton = A { initialState    :: State,     -- um estado inicial;
                     states          :: [State],           -- uma lista de estados;
                     acceptingStates :: [State],           -- os estados de aceitacao;
                     transitions     :: StateTransitions -- e as transicoes entre os estados.
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
transition aut l = do currentTransitions <- Map.lookup cs t
                      nextState          <- Map.lookup l currentTransitions
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

-- Atendendo a pedidos do opressor de terrinhas, irei exemplificar a construcao 
-- de um automato. Irei criar um automato que, dadas strings de '0's e '1's,
-- aceita apenas strings compostas exclusivamente por '0's.

-- Agora que nos temos um pouco de codigo para brincar, vamos para as questoes
-- da lista:

-- Questao 2: a logica dessas funcoes esta correta, mas elas se baseiam em uma
-- estrutura de dados muito ineficiente para lookup. Reescreva as funcoes de
-- forma que elas usem Maps em vez de listas. Sera preciso tambem redefinir os
-- tipos usados.

--- é o proprio arquivo Automato.hs

-- Questao 3: repita a questao 1, usando as definicoes da questao 2.

isParComMap = A {   initialState    = 1,
                    states          = [1,2],
                    acceptingStates = [1],
                    transitions     = Map.fromList [(1, Map.fromList[('0', 2), ('1', 2)])
												   ,(2, Map.fromList[('0', 1), ('1', 1)])]
				}

isImparComMap = A {   initialState    = 1,
                    states          = [1,2],
                    acceptingStates = [2],
                    transitions     = Map.fromList [(1, Map.fromList[('0', 2), ('1', 2)])
												   ,(2, Map.fromList[('0', 1), ('1', 1)])]
				}
--a diferença entre o par e o impar é o estado de aceitação

-- Questao 4: Defina um tipo CompositeAutomaton, que pode ser um Automaton
-- simples, um automato obtido pela uniao de dois CompositeAutomaton ou um
-- automato obtido pela intersecao de dois CompositeAutomaton. Escreva uma
-- funcao accept para CompositeAutomaton.

-- esse Int é pra diferenciar se o automato é uniao de dois CompositeAutomaton (==1)
-- ou intersecao de dois CompositeAutomaton (/=1).

data CompositeAutomaton = Simples Automaton 
						  |Construtor CompositeAutomaton CompositeAutomaton Int

acceptComp :: CompositeAutomaton -> String -> Bool
acceptComp (Simples automaton) palavra = accept automaton palavra
acceptComp (Construtor automaton1 automaton2 id) palavra = if(id == 1) 
								   then (acceptComp automaton1 palavra || acceptComp automaton2 palavra) 
								   else (acceptComp automaton1 palavra && acceptComp automaton2 palavra)
								   
-- Talvez seja uma boa ideia escrever suas respostas em um outro arquivo.
