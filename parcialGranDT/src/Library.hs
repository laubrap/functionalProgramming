{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Equipo = [Jugador]

data Jugador = Jugador{
    nombre :: String,
    velocidad :: Number, 
    habilidad :: Number,
    puesto :: String,
    partidos :: [Partido]
} deriving (Show)

data Partido = Partido{
    minutosJugados :: Number,
    golesConvertidos :: Number
} deriving (Show)

equipo = [
        Jugador "juan" 12 31 "arquero" [Partido 63 3,Partido 33 1, Partido 34 4],
        Jugador "lauti" 12 31 "defensor" [Partido 0 0,Partido 0 0, Partido 12 1],
        Jugador "jose" 12 31 "arquero" [Partido 90 3,Partido 90 1, Partido 90 4],
        Jugador "franui" 12 31 "defensor" [Partido 0 0,Partido 0 0, Partido 0 0]
 ]



jugaronMasDeNMinutos :: Equipo -> Number -> [String]
jugaronMasDeNMinutos lista n = map nombre (filter (all ((n<).minutosJugados).partidos) lista)

hayJugadorQueEmpieceConLetra :: Equipo -> Char -> Bool
hayJugadorQueEmpieceConLetra equipo letra = any ((letra==).head.nombre) equipo 

{-=====================================TECNICOS==================================-}

type Tactica = Jugador -> Jugador

data Tecnico = Tecnico {
    nombreTecnico :: String,
    tactica :: Tactica
}

agregarPorcentajeVelocidad :: Number -> Tactica
agregarPorcentajeVelocidad velocidadAgregada jugador = jugador {velocidad = (1 + velocidadAgregada/100) * velocidad jugador}

modificarHabilidad :: Number -> Tactica 
modificarHabilidad puntos jugador = jugador {habilidad =habilidad jugador + puntos }

agregarPrefijoNombre :: String -> Jugador -> Jugador
agregarPrefijoNombre prefijo jugador = jugador {nombre = prefijo ++ nombre jugador} 

tacticaBielsa :: Tactica  
tacticaBielsa = agregarPorcentajeVelocidad 50.modificarHabilidad (-10)

tacticaMenotti :: Number -> Tactica  
tacticaMenotti habilidad = (agregarPrefijoNombre "Mr.").(modificarHabilidad habilidad)

tacticaBertolotti :: Tactica  
tacticaBertolotti = tacticaMenotti 10

tacticaVanGaal :: Tactica
tacticaVanGaal jugador = jugador 

bielsa = Tecnico {
    nombreTecnico = "Bielsa",
    tactica = tacticaBielsa
}

menotti = Tecnico {
    nombreTecnico = "Menotti",
    tactica = tacticaMenotti 10
}

bertolotti = Tecnico {
    nombreTecnico = "Bertolotti",
    tactica = tacticaBertolotti
}

vanGaal = Tecnico {
    nombreTecnico = "vanGaal",
    tactica = tacticaVanGaal
}

{-====================================ES BUENO======================================-}

esBueno :: Jugador -> Bool
esBueno jugador = (habilidad jugador > velocidad jugador) || (("volante"==).puesto) jugador

mejoraEquipo :: Tecnico -> Equipo -> Bool
mejoraEquipo tecnico equipo = cuantosBuenos equipo < (cuantosBuenos.map (tactica tecnico)) equipo

cuantosBuenos :: Equipo -> Number
cuantosBuenos equipo = (length.(filter esBueno)) equipo


{-====================================ES IMPARABLE======================================-}
{-Queremos saber si un jugador es imparable. Esto se da si con el correr de los partidos 
va metiendo la misma cantidad de goles o más. Por ejemplo un jugador que en el primer 
partido no metió goles, en el segundo metió 2, en el tercero metió 2 y en el cuarto metió 
4 es imparable. En el caso de un jugador que en el primer partido no metió goles, 
en el segundo hizo un gol y en el tercero no metió goles, no es imparable.
-}

tuvoRacha :: [Partido] -> Bool
tuvoRacha (x:y:xs) = (golesConvertidos x <= golesConvertidos y) && tuvoRacha (y:xs)
tuvoRacha _ = True

jugadorEsImparable :: Jugador -> Bool
jugadorEsImparable = tuvoRacha.partidos 