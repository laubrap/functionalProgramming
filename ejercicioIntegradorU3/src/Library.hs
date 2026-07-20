module Library where
import PdePreludat

data Alumno = Alumno{
    nombre :: String,
    fechaNacimiento :: String,
    legajo :: Number,
    materias :: [String],
    criterio :: Criterio
}deriving(Show)

type Criterio = Parcial -> Bool

data Parcial = Parcial{
    cantidadPreguntas :: Number,
    materia :: String
}

cabulero :: Criterio
cabulero = odd.length.materia

hijoDelRigor :: Number -> Criterio
hijoDelRigor n = (n<).cantidadPreguntas

estudioso :: Criterio
estudioso _ = True

nico = Alumno{
    nombre ="Nico",
    fechaNacimiento = "11 enero 2003",
    legajo = 1231234,
    materias = ["Fisica", "Pdep"],
    criterio = estudioso
}

cambiarCriterio :: Criterio -> Alumno -> Alumno
cambiarCriterio nuevoCriterio alumno = alumno {criterio = nuevoCriterio}

estudiaParaParcial :: Parcial -> Alumno -> Bool
estudiaParaParcial parcial alumno = criterio alumno parcial

parcialPDP = Parcial 3 "PDP" 