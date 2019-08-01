module Main where

import System.Environment (getArgs)

import qualified CountTree as CT
import Data.Char
import Data.List (sort)

main :: IO ()
main = program0

-- O programa abaixo exibe a contagem das palavras de um programa.

program0 :: IO ()
program0 = do (fileName:_) <- getArgs
              fileContent <- readFile fileName
              let getWords = concat . map words . lines
              let countWords = foldl CT.count CT.empty (getWords fileContent)
              mapM_ (putStrLn . show) (CT.getAll countWords)
          

-- Questão 1: modifique-o, de forma que ele exiba as palavras em ordem de
-- ocorrência.

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

program1 :: IO ()
program1 = do (fileName:_) <- getArgs
              fileContent <- readFile fileName
              let getWords = concat . map words . lines
              let countWords = foldl CT.count CT.empty (getWords fileContent)
              mapM_ (putStrLn . show) (reverse(sort(map swap(CT.getAll countWords))))
          


-- Questão 2: modifique-o, fazendo com que todas as palavras contenham apenas
-- minusculas, e de forma que sejam ignorados quaisquer caracteres
-- nao-alfanumericos.

program2 :: IO ()
program2 = do (fileName:_) <- getArgs
              fileContent <- readFile fileName
              let getWords = concat . map words . lines . filter isAlphaNum . map toLower
              let countWords = foldl CT.count CT.empty (getWords fileContent)
              mapM_ (putStrLn . show) (CT.getAll countWords)
