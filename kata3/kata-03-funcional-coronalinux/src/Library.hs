module Library
    ( Persona(..),
    cuantoPonemosDePrepaga,
    genteCabulera,
    modificacionesLocas,
    alterarEmpleado
    ) where

import PdePreludat

data Persona = Persona {
  edad::Number,
  sueldo::Number,
  elementos::[String]
} deriving (Show, Eq)

perderElemento:: String -> Persona -> Persona
perderElemento nombreElemento persona = persona {
  elementos = filter (/=nombreElemento) $ elementos persona
}

cumplirAnios:: Persona -> Persona
cumplirAnios persona = persona { edad = 1 + edad persona }

incrementarSueldo:: Number -> Persona -> Persona
incrementarSueldo factor persona = persona { 
  sueldo = sueldo persona  * factor
}

modificacionesLocas:: [Persona -> Persona]
modificacionesLocas = [perderElemento "teclado", cumplirAnios , incrementarSueldo 1.2]

--aportes de prepaga: 11% de la suma de sueldos de las personas de mas de 26 años
cuantoPonemosDePrepaga:: [Persona] -> Number
cuantoPonemosDePrepaga empleados = foldr ((+).(* 0.11).sueldo) 0 (filtradoEdadMayorA26 empleados)

--si todos los elementos que poseen, tienen una longitud par
genteCabulera:: [Persona] -> [Persona]
genteCabulera gente = filter tienenLongitudPar gente

-- Aplicar modificaciones locas a una persona
alterarEmpleado:: Persona -> [Persona -> Persona] -> Persona
alterarEmpleado persona listaFunciones = foldr ($) persona listaFunciones


filtradoEdadMayorA26:: [Persona] -> [Persona]
filtradoEdadMayorA26 lista = filter ((>26).edad) lista

tienenLongitudPar:: Persona -> Bool
tienenLongitudPar listaPersonas = all (==True) (map (even.length) (elementos listaPersonas))
