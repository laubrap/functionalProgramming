module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Personaje = Personaje{
    nombre :: String,
    nivelAnsiedad :: Number,
    intereses :: [String],
    grado :: Grado
} 

data Grado =  SinEstudios | Ingenieria | Master | Doctorado | Nobel deriving(Eq,Ord,Show)

sheldon = Personaje{
    nombre = "Sheldon",
    nivelAnsiedad = 30,
    intereses = ["Star Trek","Fisica","Videojuegos","Trenes"],
    grado = Nobel
}

leonard = Personaje{
    nombre = "Leonard",
    nivelAnsiedad = 80,
    intereses = ["Videojuegos","Comics"],
    grado = Doctorado
}

howard = Personaje{
    nombre = "Howard",
    nivelAnsiedad = 90,
    intereses = ["Cinturones","Magia","Cohetes"],
    grado = Ingenieria
}

amy = Personaje{
    nombre = "Amy",
    nivelAnsiedad = 50,
    intereses = ["Arpa","Neurobiologia"],
    grado = Doctorado
}

penny = Personaje{
    nombre = "Penny",
    nivelAnsiedad = 20,
    intereses = ["Actuacion","Maquillaje"],
    grado = SinEstudios
}

type Grupo = [Personaje]

departamentoA4 :: Grupo
departamentoA4 = [leonard,sheldon]

vecinos :: Grupo
vecinos = [leonard,penny]

grupoEsRespetable :: Grupo -> Bool
grupoEsRespetable = all personajeEsRespetable 

personajeEsRespetable :: Personaje -> Bool
personajeEsRespetable  = (Ingenieria<).grado

{-punto 4-}
type Actividad = Personaje -> Personaje

variarAnsiedad :: (Number->Number) -> Personaje -> Personaje 
variarAnsiedad funcion personaje = personaje{nivelAnsiedad=(funcion.nivelAnsiedad) personaje} 

agregarInteres :: String -> Personaje -> Personaje
agregarInteres interesNuevo personaje 
                        | (notElem interesNuevo.intereses) personaje = personaje{intereses= intereses personaje ++ [interesNuevo] }
                        | otherwise                                  = personaje

nocheDeHalo :: Actividad
nocheDeHalo = variarAnsiedad (subtract 10).agregarInteres "videojuegos" 

competenciaPaintball :: Actividad
competenciaPaintball = variarAnsiedad (+20).agregarInteres "Paintball"

realizarActividad :: Actividad -> Grupo -> Grupo
realizarActividad = map 

{-punto 5-}
necesitanUnTe :: Grupo -> Grupo
necesitanUnTe = filter personajeEstaAnsioso 

personajeEstaAnsioso :: Personaje -> Bool
personajeEstaAnsioso = (80<).nivelAnsiedad

{-punto 6-}
hayAlgunObsesivoEnGrupo :: Grupo -> Bool
hayAlgunObsesivoEnGrupo = any esObsesivo

esObsesivo :: Personaje -> Bool
esObsesivo = (3<=).length.intereses

{-punto 7-}
grupoEstaEnCrisis :: Grupo -> Bool
grupoEstaEnCrisis = (300<).sum.map nivelAnsiedad

{-punto 8-}

type DelegacionCientifica = [Grupo]

potencialDeEstresEsBajo :: Grupo -> Bool
potencialDeEstresEsBajo = (200>).sum.map nivelAnsiedad.realizarActividad competenciaPaintball

esGrupoApto :: Grupo -> Bool
esGrupoApto grupo = potencialDeEstresEsBajo grupo && grupoEsRespetable grupo

gruposAptos :: DelegacionCientifica -> DelegacionCientifica
gruposAptos = filter esGrupoApto

{-punto 9-}
data Expedicion = Expedicion{
    nombreExpedicion :: String,
    delegacion :: DelegacionCientifica,
    evento :: Evento
}

type Evento = DelegacionCientifica -> DelegacionCientifica

sumarAnsiedadGrupo :: Number -> Grupo -> Grupo
sumarAnsiedadGrupo number = map (variarAnsiedad (+number)) 

tormentaDeNieve :: Evento 
tormentaDeNieve = map (sumarAnsiedadGrupo 20)

{-otra podria ser corte de calefaccion que sube la ansiedad 50 
y les agrega el interes "Abrigos"-}

{-punto 10-}
nivelPeligrosidad :: Expedicion -> Number
nivelPeligrosidad expedicion =  (sum.map (length.necesitanUnTe).evento expedicion.delegacion) expedicion

{-punto 11-}
expedicionDebeCancelarse :: Expedicion -> Bool
expedicionDebeCancelarse expedicion = ((2<).nivelPeligrosidad) expedicion || any grupoEstaEnCrisis (delegacion expedicion)