{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Library where
import PdePreludat


notaLauti = 5   
precioDolar = 1465

doble :: Number -> Number
doble numero = numero + numero

alumnoAprobo :: Number -> Bool
alumnoAprobo nota = nota >= 6

conversionDolarAPeso :: Number -> Number
conversionDolarAPeso dolares = dolares * precioDolar

maximo x y | x>y = x
           | otherwise = y


{-====================Composicion de funciones====================-}
longitudNombreEsPar :: String -> Bool
longitudNombreEsPar = even.length {-eta reduction applyed-}

siguiente :: Number -> Number
siguiente x = x + 1

siguienteEsPar :: Number -> Bool
siguienteEsPar = even.siguiente 

{-====================Ahora queremos modelas una persona con nombre y edad, y luego domicilio====================-}
type Persona = (String,Number,String) {-Tipo persona => tupla de dos elementos-}

edadPersona :: Persona -> Number
edadPersona (_,edad,_) = edad {-Al agregar domicilio esta cambio y la tupla tambien-}

esMayor :: Number -> Bool {-Estas sin embargo no cambiaron-}
esMayor x = x >= 18

personaEsMayor :: Persona -> Bool {-Estas sin embargo no cambiaron-}
personaEsMayor = esMayor.edadPersona

{-Decimos entonces que el acoplamientoe entre edadPersona y personaEsMayor es bajo
ya que al modifcar la primera, no rompe la siguiente, personaEsMayor NO necesita saber
como se comportan esMayor y edadPersona-}

{-====================Aplicacion Parcial de funciones====================-}
{-Cuando usamos aplicacion parcial, utilizamos unos parametros de base, por ejemplo, 
si quisieramos saber si un numero, que obtenemos de una tupla, es mayor a M, aplicamos
PARCIALMENTE la funcion mayor al M-}

type NumeroConNombre = (Number, String)

obtenerNum :: NumeroConNombre -> Number
obtenerNum (n,_) = n

numeroEsMayorA :: NumeroConNombre -> Number -> Bool
numeroEsMayorA num n = ((n <).obtenerNum) num

{-Acabamos de usar aplicacion parcial con composicion, ademas de usar una funcion que
recibe dos parametros en vez de uno, primero un elemento de type NumeroConNombre y luego 
el numero con el que lo compararemos-}

{-Queremos saber si una palabra comienza con una letra X-}
primeraLetra :: String -> Char
primeraLetra = head

empiezaCon :: String -> Char -> Bool
empiezaCon palabra caracter = ((caracter ==).primeraLetra) palabra
{-Es Key sensitive, no es lo mismo empezar con L que con l-}

{-====================Modelado de informacion====================-}
{-====================LISTAS====================-}
listaCorta = [1,2,3,4,5]
listaLarga = [1..20]

sumElem :: [Number] -> Number
sumElem [] = 0
sumElem (x:xs) = x + sumElem xs

{-====================TIPOS PROPIOS====================-}
{-son nuevos tipos de datos que se agregan a los existentes en el lenguaje mediante la
expresion data-}

data Humano = Humano String Number
{-Un tipo Humano con un constructor llamado Humano que tiene un String y un Numero-}

humanoEsMayor :: Humano -> Bool
humanoEsMayor = (18<=).edad

edad :: Humano -> Number 
edad (Humano _ edad) = edad

{-====================Uso record Syntax====================-}
{-Que pasa si queremos agregarle mas parametros a nuestro data? romperiamos edad, 
ademas de volverlo menos expresivo, imaginemos que agregamos
domicilio-String
teléfono-String
fecha de nacimiento-tupla de tres elementos
es buena persona-bool
plata que tiene en el bolsillo-decimales-}

data Human = Human String Number String String (Number, Number, Number) Bool Number

{-Mucho mas engorrosa, sin contar que necesitaremos una funcion para extraer cada
parametro. Para esto tenemos la notacion RECORD SYNTAX-}

data Pers = Pers{
    nombre :: String,
    edadPers :: Number,
    domicilio :: String,
    telefono :: String,
    fechaNacimiento :: (Number, Number, Number),
    buenaPersona :: Bool,
    plata :: Number
} deriving(Show)

{-Ahora adquirimos funciones llamadas como el parametro, por lo que, primero creemos
una Persona-}

lauti = Pers{
    nombre = "Lauti",
    edadPers = 20,
    domicilio = "av san juan",
    telefono = "11243141",
    fechaNacimiento = (11, 01, 2005),
    buenaPersona = True,
    plata = 2000
} 

{-si queremos ahora saber si una persona es mayor usamos la funcion edadPers generada
por el record syntax para saber la hora-}

persEsMayor :: Pers -> Bool
persEsMayor = (18<=).edadPers
