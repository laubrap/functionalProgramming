{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Library where
import PdePreludat

type Habilidad = Personaje -> Personaje

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Habilidad]
    }deriving (Show,Eq)

data Personaje = Personaje {
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving (Show,Eq)

data Universo = Universo {
    personajes :: [Personaje]
} deriving (Show,Eq)

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo | puedeChasquear guantelete = reducirUniverso universo
                              | otherwise                 = universo
                              
reducirUniverso :: Universo -> Universo
reducirUniverso universo = universo {personajes = take (((flip div 2).cantidadPersonajes) universo) (personajes universo)}


puedeChasquear :: Guantelete -> Bool
puedeChasquear guantelete = (("uru"==).material) guantelete && ((==6).length.gemas) guantelete

cantidadPersonajes :: Universo -> Number
cantidadPersonajes universo = (length.personajes) universo

{-===================================Punto 2=========================================-}

aptoParaPendex :: Universo -> Bool
aptoParaPendex universo =  all ((45<).edad) (personajes universo)

energiaTotal :: Universo -> Number 
energiaTotal universo = sumOf energia (personajes universo)

{-===================================Punto 2=========================================-}

bajarEnergia :: Number -> Personaje -> Personaje
bajarEnergia cantidad personaje = personaje {energia = energia personaje - cantidad}

quitarHabilidad :: String -> Personaje -> Personaje
quitarHabilidad habilidadAQuitar personaje = 
    personaje {habilidades = filter (/=habilidadAQuitar) (habilidades personaje)}

transportarAPlaneta :: String -> Personaje -> Personaje
transportarAPlaneta nuevoPlaneta personaje = personaje {planeta = nuevoPlaneta}

quitarTodaEnergia :: Personaje -> Personaje 
quitarTodaEnergia personaje = personaje {energia = 0}

quitarTodasLasHabilidades :: Personaje -> Personaje 
quitarTodasLasHabilidades personaje = personaje {habilidades = []}

reducirEdad :: Personaje -> Personaje
reducirEdad personaje = personaje {edad = div (edad personaje) 2}

mente :: Number -> Habilidad
mente cantidad personaje = bajarEnergia cantidad personaje

alma :: String -> Habilidad
alma habilidadAquitar personaje = ((quitarHabilidad habilidadAquitar).(bajarEnergia  10)) personaje

espacio :: String -> Habilidad
espacio planeta = (transportarAPlaneta planeta).(bajarEnergia 20)

poder :: Habilidad
poder personaje 
    | (length.habilidades) personaje <= 2 = (quitarTodaEnergia.quitarTodasLasHabilidades) personaje
    | otherwise                           = quitarTodaEnergia personaje

tiempo :: Habilidad 
tiempo personaje = bajarEnergia 50 personaje {edad = (max 18.edad.reducirEdad) personaje}  

loca :: Habilidad -> Habilidad
loca gema = gema . gema

guanteleteGoma = Guantelete {
    material = "goma",
    gemas = [tiempo,alma "usar mjolnir",loca (alma "programación en Haskel")]
 }

utilizar :: [Habilidad] -> Personaje -> Personaje
utilizar habilidades personaje = foldr ($) personaje habilidades

gemaMasPoderosa :: Guantelete -> Personaje -> Habilidad
gemaMasPoderosa guantelete personaje = gemaQueSacaMas (gemas guantelete) personaje

gemaQueSacaMas :: [Habilidad] -> Personaje -> Habilidad
gemaQueSacaMas [poder] _ = poder
gemaQueSacaMas (x:y:xs) personaje 
    | (energia.x) personaje < (energia.y) personaje = gemaQueSacaMas (x:xs) personaje  
    | otherwise                                     = gemaQueSacaMas (y:xs) personaje  



