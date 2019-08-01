module Main where

import Aula20

import Control.DeepSeq

import Control.Parallel
import Control.Parallel.Strategies

main :: IO ()
main = program1

-- Questao 1

program1 :: IO ()
program1 = print $ mySum2 [1..1000000000]

-- Use mySum' para definir cada uma das somas.
mySum2 :: [Int] -> Int
mySum2 xs = runEval $ do x <- rpar $ mySum' l
                         y <- rpar $ mySum' r
                         return (x+y) where (l,r) = disjoin xs

-- Questao 2

program2 :: IO ()
program2 = print $ mySum3 [1..100000000]

mySum3 :: [Int] -> Int
mySum3 xs = runEval $ do x <- rpar $ mySum' l
                         y <- rpar $ mySum' ls
                         z <- rpar $ mySum' r
                         w <- rpar $ mySum' rs
                         return (x+y+z+w) where (l,ls,r,rs) = disjoin2 xs

disjoin2 :: [a] -> ([a], [a], [a], [a])
disjoin2 []               = ([], [],[],[])
disjoin2 [x]              = ([x], [],[],[])
disjoin2 [x,x2]           = ([x], [x2],[],[])
disjoin2 [x,x2,x3]        = ([x], [x2],[x3],[])
disjoin2 (x:x2:x3:x4:xs)  = (x:l, x2:ls, x3:r, x4:rs) where
                            (l,ls,r,rs) = disjoin2 xs
