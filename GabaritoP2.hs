module Gabarito where

import Control.Applicative
import Control.Monad

import InfinityTree

-- Questao 1

data SeqTree a = SEmpty
               | SLeaf a
               | SBranch Int Int Int (SeqTree a) (SeqTree a)

instance Functor SeqTree where
        fmap f SEmpty                = SEmpty
        fmap f (SLeaf x)             = SLeaf (f x)
        fmap f (SBranch s c h lt rt) = SBranch s c h (fmap f lt) (fmap f rt)

-- Questao 2

primes :: [Int]
primes = sieve $ 2:odds where
         odds         = iterate (+2) 3
         sieve []     = []
         sieve (x:xs) = x : filter ((>0) . (`mod` x)) (sieve xs)

decompose :: Int -> [Int]
decompose x = dcp x primes where
              dcp 1 ps = []
              dcp x ps = q : dcp (x `div` q) qs where
                         qs = dropWhile (not . divides x) ps
                         q  = head qs
                         divides a = (== 0) . (mod a)

-- Questao 3

data BSTree k v = Empty | BBranch k v (BSTree k v) (BSTree k v)

-- Basta usar a lista como se fosse uma fila. Ou implementar uma fila mais eficiente.
breadthWalk :: BSTree k v -> [v]
breadthWalk tree = walk tree [] where
                   walk Empty []                            = []
                   walk Empty (tree:trees)                  = walk tree trees
                   walk (BBranch key val lt rt) []           = val : walk lt [rt]
                   walk (BBranch key val lt rt) (tree:trees) = val : walk tree (trees ++ [lt, rt])

-- Questao 4
-- Lol parece que o uso de <|> jah faz essa funcao se tornar eficiente
-- Essencialmente, quase nenhuma chamada vai se desdobrar em duas chamadas
-- recursivas.
decompose37 :: Int -> Maybe (Int, Int)
decompose37 0 = Just (0, 0)
decompose37 x = try3 <|> try7 where
                try3                  = (.+.) <$> pure (1, 0) <*> try (x - 3)
                try7                  = (.+.) <$> pure (0, 1) <*> try (x - 7)
                try a                 = guard (a >= 0) *> decompose37 a
                (x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

-- Questao 5
-- (Vou fazer usando listas. Eh facil adaptar a logica para InfinityTree)

memoDecompose37 :: Int -> Maybe (Int, Int)
memoDecompose37 = search (f <$> naturalsTree) where
                  f 0 = Just (0, 0)
                  f x = try3 <|> try7 where
                        try3  = (.+.) <$> pure (1, 0) <*> try (x - 3)
                        try7  = (.+.) <$> pure (0, 1) <*> try (x - 7)
                        try a = guard (a >= 0) *> memoDecompose37 a
                        (x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)

-- Questao 6
-- Vou chamar de .=<<. para nao dar conflito com o operador definido pela
-- linguagem.

f .=<<. x = join $ pure f <*> x
