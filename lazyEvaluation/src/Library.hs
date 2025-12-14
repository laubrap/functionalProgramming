module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{-===========================LAZY EVALUATION===========================-}
{-Vamos a implementar primero la funcion doblePrimero, que dados dos numeros nos
devuelve.... si, el doble del primero-}

doblePrimero :: Number -> Number -> Number
doblePrimero a b = 2*a

{-podemos ahora evaluar varias formas, primero haremos doblePrimero 5 7 devolviendonos
    ghci> 10
-}
{-Que pasa si ahora ejecutamos la funcion pero con doblePrimero 5 (1/0)
Bien sabemos que matematicamente no es valida, al usar solamente el primer numero, nunca
se evaluara 1/0, ya que usa los elementos A MEDIDA que los necesita  -}

esPrimo :: Number -> Bool
esPrimo 1 = False
esPrimo 2 = True
esPrimo n = noTieneDivisores 2 (n -1) n

noTieneDivisores :: Number -> Number -> Number -> Bool
noTieneDivisores minimo maximo n 
    | mod n minimo == 0  = False
    | minimo == maximo   = True
    | otherwise          = noTieneDivisores (minimo + 1) maximo n

primerosPrimos :: Number -> [Number]
primerosPrimos n = (take n.filter esPrimo) [1..]
