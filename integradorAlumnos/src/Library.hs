{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


{-====================Ejercicio Integrador====================
Modelar un alumno, que define 
un nombre, 
la fecha de nacimiento, 
el legajo (sin dígito verificador), 
las materias que cursa 
y el criterio para estudiar ante un parcial:
algunos son estudiosos: estudian siempre, 
otros son hijos del rigor: estudian si el parcial tiene más de n preguntas, 
y también están los cabuleros, que estudian si la materia tiene una cantidad impar de letras. 
-}

data Alumno = Alumno{
    nombre :: String,
    fechaNacimiento :: (Number, Number, Number),
    legajo :: Number,
    materiasQueCursa :: [String],
    criterio :: Criterio
} deriving(Show)

data Parcial = Parcial{
    materia :: String,
    cantidadPreguntas :: Number
}

type Criterio = Parcial -> Bool

estudioso :: Criterio
estudioso _ = True

hijoDelRigor :: Number -> Criterio
hijoDelRigor n (Parcial _ preguntas) = preguntas > n

cabulero :: Criterio
cabulero = odd.length.materia 

nico = Alumno {
    nombre = "Nico",
    fechaNacimiento = (10, 3, 1993),
    legajo = 124124,
    materiasQueCursa = ["sysop", "proyecto"],
    criterio = hijoDelRigor 7
}

parcialPdep = Parcial "Paradigmas" 2

estudiaPara :: Alumno -> Parcial -> Bool
estudiaPara alumno parcial = (criterio alumno) parcial 

{-osea ahora en vez de usar una funcion sola, usamos la funcion que TIENE el alumno, a
traves de la notacion record syntax que nos da servidas las funciones para acceder a los
campos del Data-}

{-====================Cambiar a nico de criterio de estudio====================-}
{-En realidad sabemos que no cambiamos el estado interno real de nuestro Alumno nico,
como lo podriamos hacer en C-}

cambiarCriterio :: Criterio -> Alumno -> Alumno
cambiarCriterio nuevoCriterio alumno = alumno {criterio = nuevoCriterio}

{-sin embargo no podemos hacer un cambio de criterio a nico y luego evaluar a nico de nuevo
esperando la respuesta de true (al pasar a ser estudioso) ya que contamos con INMUTABILIDAD
y debemos evaluar asi: estudiaPara (cambiarCriterio estudioso nico) parcialPdep usando
lo que transformamos-}
