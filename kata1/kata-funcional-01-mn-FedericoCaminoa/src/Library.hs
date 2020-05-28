module Library
    ( calcuLoco
    ) where

import PdePreludat

-- si el primer número es mayor que el segundo, devuelve la división de ambos números
-- si no, si el primer número es impar, devuelve el segundo número menos 5
-- en caso contrario, devuelve el doble del primer número 

calcuLoco :: Number -> Number -> Number
calcuLoco numero1 numero2
    | numero1 > numero2 = numero1 / numero2
    | odd numero1       = numero2 - 5
    | otherwise         = 2 * numero1

