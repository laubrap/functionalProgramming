{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

siguiente = (\numero numero2-> numero + numero2) 1

suma :: Number -> Number -> Number
suma a b = a+b