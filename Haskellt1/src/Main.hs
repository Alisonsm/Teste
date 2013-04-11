module Main where

main::IO()
main = undefined

-- 1.Escreva uma função recursiva strSizes :: [String] -> [Int], que receba uma lista de strings e retorne uma lista com os tamanhos dessas strings.
strSizes :: [String] -> [Int]
strSizes [] = []
strSizes lst = length (head lst) : strSizes (tail lst) 

-- 2.Crie uma função recursiva charFound :: Char -> String -> Bool, que indique se o primeiro argumento está contido na lista 
-- passada como segundo argumento.
charFound :: Char -> String -> Bool
charFound c "" = False
charFound c s
        | c == head s = True
        | otherwise = charFound c (tail s)
        
-- 3.Defina uma função recursiva removeSpaces :: String -> String, que receba uma string e retorne uma string equivalente, 
-- sem espaços em branco.
removeSpaces :: String -> String
removeSpaces "" = ""
removeSpaces s = if (head s) /= ' ' 
        then (head s) : removeSpaces (tail s) 
        else removeSpaces (tail s)
        
-- 4.Escreva uma função htmlLink :: (String, String) -> String, que receba uma tupla contendo um nome e uma URL e 
-- retorne esses dados em uma string formatada como um link HTML.
htmlLink :: (String, String) -> String
htmlLink (x,y) = "<A HREF=\"" ++ y ++ ['"', '>'] ++ x ++ "</A>"

-- 5.Crie uma função recursiva htmlListItems :: [String] -> [String], que receba uma lista de strings e retorne outra lista 
-- contendo as strings formatadas como itens de lista em HTML.
htmlListItems :: [String] -> [String]
htmlListItems [] = []
htmlListItems (s:x) = ("<LI>" ++ s ++ "</LI>") : htmlListItems x

-- 6.Escreva uma função recursiva maiorElem :: [Int] -> Int que retorne o maior número contido na lista passada como argumento.
maiorElem :: [Int] -> Int
maiorElem (s:x) 
        | x == [] = s
        | x /= [] && s > head x =  maiorElem (s:(tail x))
        | s < head x = maiorElem x
        
-- 7.Defina uma função recursiva intervalo :: Int -> Int -> Int -> [Int], que receba 3 inteiros a, b, p e retorne uma lista 
-- contendo números no intervalo [a..b], com passo p.
intervalo :: Int -> Int -> Int -> [Int]
intervalo a b c = if b > a
        then a: intervalo (a+c) b c
        else []
        
-- 8.Escreva uma função recursiva mapfxy :: [(Int,Int)] -> [Int], que receba uma lista de tuplas de forma (x,y) 
-- e retorne uma lista contendo o valor de 2*x + y para cada tupla.
mapfxy :: [(Int,Int)] -> [Int]
mapfxy [] = []
mapfxy (s:x) = 2 * fst (s) + snd (s) : mapfxy x

-- 9.Escreva outra versão da função mapfxy, desta vez definindo e usando uma função auxiliar auxFunc :: (Int,Int) -> Int.
auxFunc :: (Int,Int) -> Int
auxFunc (x,y) = (2*x + y)

mapf :: [(Int,Int)] -> [Int]
mapf [] = []
mapf s = auxFunc (head s) : mapf (tail s)

-- 3
somatorio :: [Int] -> Int
somatorio [] = 0
somatorio lis = head lis + somatorio (tail lis)

