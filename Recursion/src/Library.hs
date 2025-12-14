{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * factorial (n-1) 

fibonacci :: Number -> Number
fibonacci 0 = 0 
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci(n-2)

esPrimo :: Number -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo n = noTieneDivisores 2 (n -1) n

noTieneDivisores :: Number -> Number -> Number -> Bool
noTieneDivisores minimo maximo n 
    | mod n minimo == 0  = False
    | minimo == maximo   = True
    | otherwise          = noTieneDivisores (minimo + 1) maximo n

{-=========================Self development of lists functions using recursion=========================-}

long :: [a]-> Number
long [] = 0
long (x:xs) = 1 + long xs

sumList :: [Number]-> Number
sumList [] = 0
sumList (x:xs) = x + sumList xs

ultimoList :: [a]-> a
ultimoList [a] = a
ultimoList (x:xs) = ultimoList xs

tomar :: Number -> [a] -> [a]
tomar n _ | n<= 0 = []
tomar _ [] = []
tomar n (x:xs) = x : tomar (n-1) xs  

sacar :: Number -> [a] -> [a]
sacar 0 xs = xs
sacar n (_:xs) = sacar (n-1) xs  

elemento :: [a] -> Number -> a
elemento (x:_) 0 = x
elemento (_:xs) n = elemento xs (n - 1)

estaEnLista :: Eq a => a -> [a] -> Bool
estaEnLista _ [] = False
estaEnLista e (x:xs) = e==x || estaEnLista e xs

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:y:ys) 
    | x > y     = maximo (x:ys)
    | otherwise = maximo (y:ys)

