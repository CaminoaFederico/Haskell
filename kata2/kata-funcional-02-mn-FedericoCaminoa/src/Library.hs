module Library
    ( nombre,
    cantidadParciales,
    materiaCopada,
    tieneProblemasDeCupo
    ) where

import PdePreludat

type Nombre = String
type CantidadInscriptos = Number

type Materia = (Nombre, CantidadInscriptos)

nombre :: Materia -> Nombre
nombre = fst

cantidadParciales :: Materia -> CantidadInscriptos
cantidadParciales = snd

materiaCopada :: Materia -> Bool
materiaCopada = even . length . nombre

tieneProblemasDeCupo :: Materia -> Bool
tieneProblemasDeCupo = (> 2500) . cantidadParciales 
