{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use maximum" #-}
module Library where
import PdePreludat


doble :: Number -> Number
doble numero = numero + numero

{-Nos piden tres requerimientos:
necesitamos saber qué clientes nos deben más de $ 10.000
también qué clientes tienen un nombre palíndromo (capicúa)
y qué clientes tienen alguna factura de exactamente $ 500.000
-}

data Cliente = Cliente {
    nombre :: String,
    deuda :: Number,
    facturas :: [Number]
} deriving (Show)

clientes = [
    Cliente "Biasutto" 6000 [4000, 5000],
    Cliente "Colombatti" 15000 [30000],
    Cliente "Marabotto" 200 [500000, 140000],
    Cliente "neuquen" 200 [500000, 140000]
 ]

{-================================Sin usar orden superior================================-}
{-================================Debe mas de cierta cant================================-}
clientesQueDebenMasDe :: Number -> [Cliente] -> [Cliente]
clientesQueDebenMasDe _ [] = []
clientesQueDebenMasDe monto (x:xs) | deuda x > monto = x : clientesQueDebenMasDe monto xs
                               | otherwise = clientesQueDebenMasDe monto xs

{-================================Nombre palindromo de cli================================-}

esPalindromo :: String -> Bool
esPalindromo nombre = nombre == reverse nombre

clientesNombrePalindromo :: [Cliente] -> [Cliente]
clientesNombrePalindromo [] = []
clientesNombrePalindromo (x:xs) | (esPalindromo.nombre) x = x: clientesNombrePalindromo xs
                                | otherwise = clientesNombrePalindromo xs

{-================================Clientes con una factura >5000================================-}


clientesConfacturaIgualA :: Number -> [Cliente] -> [Cliente]

clientesConfacturaIgualA _ [] = []
clientesConfacturaIgualA monto (x:xs) | (elem monto.facturas) x = x : clientesConfacturaIgualA monto xs
                                  | otherwise           = clientesConfacturaIgualA monto xs

{-Aca vemos como se produce una repeticion de codigo, ya que en las tres soluciones de 
los requerimientos tenemos nuestro corte en lista vacia, y separamos en cabeza cola, 
quedandonos con la cabeza si cumple una cierta condicion-}

{-================================Orden Superior================================-}

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar criterio [] = []
filtrar criterio (x:xs) | criterio x = x : filtrar criterio xs
                        | otherwise           = filtrar criterio xs

transformar :: (a -> b) -> [a] -> [b]
transformar funcion [] = []
transformar funcion (x:xs) = funcion x : transformar funcion xs

--Del AND al ALL
tambien :: [Bool] -> Bool
tambien [] = True
tambien (x:xs) = x && tambien xs

todos :: (a -> Bool) -> [a] -> Bool
todos funcion lista = (and.transformar funcion) lista

--Del OR al ANY
orSelf :: [Bool] -> Bool
orSelf [] = False
orSelf (x:xs) = x || orSelf xs

alguno :: (a -> Bool) -> [a] -> Bool
alguno funcion lista = (orSelf.transformar funcion) lista

--SUMOF transforma una lista y luego suma los elem
suma :: (a -> Number) -> [a] -> Number
suma funcion lista = (sum.transformar funcion) lista

--Funcion que invierte los parametros, si tenemos un f :: Lista -> Number -> Number
--podemos invertir y que quede f :: Number -> Lista -> Number
flippear :: (a -> b -> c) -> b -> a -> c
flippear f a b = f b a

--nos sirve por si no podemos cambiar una funcion, ya que no pertenece a nuestra biblioteca
--y debemos confeccionar una funcion que reduce los parametros de otra forma

{-Fold -> nos permite combinar o reducir expresiones hasta obtener un valor irreductible
recibe una operacion binaria-}

sumarAizquierda :: [Number] -> Number
sumarAizquierda lista = foldl1 (+) lista

{-en este caso, al ser foldl1 lo hace de izquierda a derecha y la lista debe tener AL MENOS 
un elemento
en una lista compuesta por [1,2,3] hace 1+2 = 3 y luego 3+3 = 6, en una operacion
conmutativa no vemos el cambio, sin embargo, con la division por ejemplo, el resultado
no es el mismo-}

maxLista :: [Number] -> Number
maxLista lista = foldl1 max lista

{-Lo mismo sucede con la funcion foldr1-}

sumarAderecha :: [Number] -> Number
sumarAderecha lista = foldr1 (+) lista

{-Si quisieramos sumar la longitudes de nombres, no podriamos, ya que recibimos una lista
de strings, y FOLDR/FOLDL1 no permiten cambiar el tipo, si tenemos entrada [String] la 
salida sera un String -}

{-en este caso, al ser foldr1 lo hace de derecha a izquierda y la lista debe tener AL MENOS 
un elemento
en una lista compuesta por [1,2,3] hace 3+2 = 5 y luego 5+1 = 6, en una operacion
conmutativa no vemos el cambio, sin embargo, con la division por ejemplo, el resultado
no es el mismo si hacemos foldr1 o foldl1-}

{-==============================Funciones mas generales => foldr y foldl ==============================
==============================================FOLDR====================================================
recibe: funcionBinaria semilla(Mismo tipo que el output de la funcion) lista
En este caso si nos permite cambiar el tipo de salida, siempre y cuandoe este sea del mismo
tipo que su semilla-}

sumarLongitudesDePalabrasADer :: [String] -> Number
sumarLongitudesDePalabrasADer lista = foldr ((+).length) 0 lista

{-===================================Total saldo clientes a derecha==============================-}

data Cli = Cli{
    nomCli :: String,
    saldoCli :: Number
}

clientes2 = [
    Cli "Amex" 500,
    Cli "Sura" 700,
    Cli "Pirex" 550
 ]

--Usamos semilla 
totalSaldo :: [Cli] -> Number
totalSaldo lista = foldr ((+).saldoCli) 0 lista


{-=====================================FUNCION $======================================
====================1) Nos sirve para aplicar una funcion de un solo parametros================-}

identidad :: Number -> Number
identidad n = n

{-En terminal identidad $ 4 devuelve 4-}

{-===============================2) Evitar el uso de parentesis===========================-}
tomarListaPares :: Number -> [Number]
tomarListaPares n = take n $ filter even [1..100]

{-En vez de lo siguiente-}
tomarListaPares2 :: Number -> [Number]
tomarListaPares2 n = take n (filter even [1..100])

{-=======================================Foldr + $=======================================-}
{-Nos sirve para aplicar un conjunto de funciones-}

data Persona = Persona {
    edad :: Number,
    felicidad :: Number,
    amigos :: [String]
} deriving(Show)

cumplirAnios :: Persona -> Persona
cumplirAnios persona = persona {edad = edad persona + 1}

caminar :: Number -> Persona -> Persona
caminar kms persona = persona {felicidad = felicidad persona + kms*2 }

hacerseAmigoDe :: String -> Persona -> Persona
hacerseAmigoDe nuevoAmigo persona = persona {amigos = nuevoAmigo:amigos persona}

persona = Persona 29 90 ["juan"]


