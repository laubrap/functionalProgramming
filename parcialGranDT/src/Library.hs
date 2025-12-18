module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Puesto = String

data Jugador = Jugador{
    nombre :: String,
    velocidad :: Number, 
    habilidad :: Number,
    puesto :: Puesto,
    partidos :: [Partido]
}

data Partido = Partido{
    minutosJugados :: Number,
    golesConvertidos :: Number
}

jugadoresConMasDeNMinutos :: [Jugador] -> Number -> [Jugador]
jugadoresConMasDeNMinutos lista n  = filter (all (n<.minutosJugados) partidos) lista