{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Hoist not" #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module Library where
import PdePreludat

data Atraccion = Atraccion {
    nombre :: String,
    alturaMinima :: Number,
    duracion :: Number,
    opiniones :: [String],
    estaEnMantenimiento :: Bool,
    reparaciones :: [Reparacion]
}

data Reparacion = Reparacion {
    duracionReparacion :: Number,
    trabajo :: Trabajo
}

puntosDeBuena :: Atraccion -> Number
puntosDeBuena atraccion
 | duracionEsProlongada atraccion             = 100
 | tienePocasReparacionesPendientes atraccion = (puntosPorNombre 10.nombre) atraccion + (puntosPorOpinion 2.opiniones) atraccion
 | otherwise                                  = ((10*).alturaMinima) atraccion

duracionEsProlongada :: Atraccion -> Bool
duracionEsProlongada atraccion = ((>10).duracion) atraccion

tienePocasReparacionesPendientes :: Atraccion -> Bool
tienePocasReparacionesPendientes atraccion = ((3>).length.reparaciones) atraccion

puntosPorNombre :: Number -> String -> Number
puntosPorNombre puntos nombre = ((*puntos).length) nombre

puntosPorOpinion :: Number -> [String] -> Number
puntosPorOpinion puntos opiniones =  ((*puntos).length) opiniones 

{-===================================TECNICOS======================================-}

type Trabajo = Atraccion -> Atraccion

eliminarUltimaReparacion :: Trabajo
eliminarUltimaReparacion atraccion = atraccion {reparaciones = (init.reparaciones) atraccion}

actualizarEstado :: Trabajo 
actualizarEstado atraccion = atraccion {estaEnMantenimiento = (not.null.reparaciones) atraccion}

realizarTrabajo :: Trabajo
realizarTrabajo = actualizarEstado.eliminarUltimaReparacion

prolongarDuracion :: Number -> Trabajo
prolongarDuracion cantidad atraccion = atraccion {duracion = ((*cantidad).duracion) atraccion}

agregarOpinion :: String -> Trabajo
agregarOpinion opinion atraccion = atraccion {opiniones = opinion : opiniones atraccion}

agregarAlturaMinima :: Number -> Trabajo
agregarAlturaMinima cantidad atraccion = atraccion {alturaMinima = ((cantidad+).alturaMinima) atraccion}

ajusteDeTornilleria :: Number -> Trabajo
ajusteDeTornilleria tornillos atraccion = 
    (realizarTrabajo.prolongarDuracion (min 10 tornillos)) atraccion

engrase :: Number -> Trabajo
engrase cantidadGrasa atraccion = 
    (realizarTrabajo.agregarOpinion "para valientes".agregarAlturaMinima (0.1*cantidadGrasa)) atraccion


mantenimientoElectrico :: Trabajo
mantenimientoElectrico atraccion = realizarTrabajo atraccion {opiniones = (take 2.opiniones) atraccion}

mantenimientoBasico :: Trabajo 
mantenimientoBasico atraccion = (engrase 10.ajusteDeTornilleria 8) atraccion

{-=====================================Punto 3===================================-}

meDaMiedito :: Atraccion -> Bool
meDaMiedito atraccion =  (any ((>4).duracionReparacion).reparaciones) atraccion

cerramosAtraccion :: Atraccion -> Bool
cerramosAtraccion atraccion = 7 < (sumOf (duracionReparacion).reparaciones) atraccion

disneyNoExistis :: [Atraccion] -> Bool
disneyNoExistis atracciones = all (not.null.reparaciones) $ filter ((5<).length.nombre) atracciones

{-================================Reparaciones peolas==========================================-}

reparacionesPeolas :: Atraccion -> [Reparacion] -> Bool
reparacionesPeolas _ [] =True
reparacionesPeolas _ [_] = True
reparacionesPeolas atraccion (x:y:xs) = 
    (puntosDeBuena.(trabajo y)) atraccion > (puntosDeBuena.(trabajo x)) atraccion 
    && reparacionesPeolas atraccion (y:xs)

tieneReparacionesPeolas :: Atraccion -> Bool
tieneReparacionesPeolas atraccion = reparacionesPeolas atraccion (reparaciones atraccion) 

{-================================Many a la obra==========================================-}
realizarTrabajosPendientes :: Atraccion -> Atraccion
realizarTrabajosPendientes atraccion  = 
   foldr ($) atraccion (map (trabajo) $ reparaciones atraccion)




