module Library where
import PdePreludat

data Guerrero = Guerrero{
    nombre :: String,
    nivelFuerza :: Number,
    raza :: String,
    transformaciones :: Number
}deriving(Show)

goku = Guerrero{
    nombre = "Goku",
    nivelFuerza = 300,
    raza = "Saiyajin",
    transformaciones = 1
}

piccolo = Guerrero{
    nombre = "Piccolo",
    nivelFuerza = 150,
    raza = "Namekusein",
    transformaciones = 2
}

yamcha = Guerrero{
    nombre = "Yamcha",
    nivelFuerza = 50,
    raza = "Humano",
    transformaciones = 0
}

freezer = Guerrero{
    nombre = "Freezer",
    nivelFuerza = 10000,
    raza = "Demonio",
    transformaciones = 1
}

dodoria = Guerrero{
    nombre = "Dodoria",
    nivelFuerza = 10000,
    raza = "Demonio",
    transformaciones = 1
}

comerSemillaErmitanio :: Guerrero -> Guerrero
comerSemillaErmitanio guerrero = guerrero{
            nivelFuerza=4000+nivelFuerza guerrero,
            nombre = "Super " ++ nombre guerrero
            }

transformar :: Guerrero -> Guerrero
transformar guerrero 
        | transformaciones guerrero < 3 = guerrero{
            nivelFuerza= nivelFuerza guerrero + (incrementoTansformacion.raza) guerrero,
            transformaciones = ((1+).transformaciones) guerrero
            }
        | otherwise = guerrero

incrementoTansformacion :: String -> Number
incrementoTansformacion "Saiyajin" = 2000
incrementoTansformacion "Demonio" = 5000
incrementoTansformacion _ = 100

guerreroGanador :: Guerrero -> Guerrero -> Guerrero
guerreroGanador guerrero1 guerrero2 
                    | (nivelFuerza.transformar) guerrero1 >= (nivelFuerza.transformar) guerrero2 = guerrero1
                    | otherwise                                                                  = guerrero2

type Equipo = [Guerrero]
lider = head

guerrerosZ :: Equipo
guerrerosZ = [goku,yamcha,piccolo]  


fuerzasFreezer :: Equipo
fuerzasFreezer = [freezer,dodoria]

agregarGuerreroEquipo :: Guerrero -> Equipo -> Equipo
agregarGuerreroEquipo guerrero equipo | nivelFuerza guerrero >= ((/2).nivelFuerza.lider) equipo = equipo ++ [guerrero]
                                      | otherwise                                               = equipo        