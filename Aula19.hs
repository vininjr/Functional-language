module Aula19 where

import Control.Applicative
import Control.Monad

-- Questao 1
-- A funcao abaixo deve ser equivalente a (<$>), mas restrita a Applicative.
-- Escreva-a usando pure e <*>.

(.<$>.) :: Applicative f => (a -> b) -> f a -> f b
f .<$>. x = pure f <*> x

--(<$>) :: Functor f => (a -> b) -> f a -> f b
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b
--pure :: Applicative f => a -> f a


data MyMaybe a = Some a | None deriving Show

-- Questao 2
-- Faca MyMaybe uma instancia de Functor.
instance Functor MyMaybe where
         fmap f None = None
	 fmap f (Some a) = Some (f a)
--fmap :: Functor f => (a -> b) -> f a -> f b


-- Questao 3
-- Faca MyMaybe uma instancia de Applicative.
instance Applicative MyMaybe where
	None <*> None = None
	None <*> Some a = None
	Some a <*> None = None
	Some f <*> Some a = Some (f a)
	
	pure a = Some a 
 
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b
--pure :: Applicative f => a -> f a


-- Questao 4
-- Faca MyMaybe uma instancia de Alternative.
instance Alternative MyMaybe where
	None <|> None = None
	None <|> Some a = Some a
	Some a <|> None = Some a
	Some a <|> Some b = Some a
	
	empty = None

-- Questao 5
-- Faca MyMaybe uma instancia de Monad.
-- (Apesar de nos termos aprendido sobre =<<, a definicao de uma monada envolve
-- o operador (>>=) = filp (=<<))
instance Monad MyMaybe where
	None >>= f = None
	Some a >>= f =  (f a) 


-- Questao 6
-- Agora que vc definiu MyMaybe como uma monada, experimente a funcao join.
-- Em nenhum momento nos definimos join diretamente, o que nos leva a crer que
-- join pode ser definida em termos de (>>=). Defina myJoin, que deve ser
-- equivalente a join.

myJoin :: Monad m => m (m a) -> m a
myJoin f = f >>= id
