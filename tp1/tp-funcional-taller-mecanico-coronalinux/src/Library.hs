module Library where
import PdePreludat

type Desgaste = Number
type Patente = String
type Fecha = (Number,Number,Number)

--Definiciones base
anio :: Fecha -> Number
anio (_,_,year) = year

data Auto = Auto {
    patente :: Patente,
    desgasteLlantas :: [Desgaste],
    rpm :: Number,
    temperaturaAgua :: Number,
    ultimoArreglo :: Fecha
} deriving Show

volkswagen = Auto {
    patente = "AD012ER",
    desgasteLlantas = [0.5,0.1,0.6,0.4],
    rpm = 2000,
    temperaturaAgua = 80,
    ultimoArreglo = (10,12,2016)
}
ford = Auto {
    patente = "DJV214",
    desgasteLlantas = [0.51,0.1,0.6,0.4],
    rpm = 2400,
    temperaturaAgua = 200,
    ultimoArreglo = (10,11,2015)
}
renault = Auto {
    patente =  "DJV215",
    desgasteLlantas = [0,0,0,0],
    rpm = 2000,
    temperaturaAgua = 90,
    ultimoArreglo = (15,4,2010)
}
chevrolet = Auto {
    patente = "DFH029",
    desgasteLlantas = [0,0,0,0],
    rpm = 3000,
    temperaturaAgua = 100,
    ultimoArreglo = (6,2,2020)
}

-- Punto 1 (Costo reparacion coche)
-- si la patente tiene 7 digitos = $12500,
-- si la patente tiene 6 digitos, si la patente está entre
--     las letras "DJ" y "NB", se aplica el calculoPatental
--            (3000 * longitud para las patentes que terminen en 4)
--            (o 20000 para el resto de las patentes)
-- en caso de no entrar en los puntos anteriores = 15000
type Precio = Number

calculoPatental :: Patente -> Precio
calculoPatental patente | ((== "4").drop 5) patente = ((* 3000).length) patente
                        | otherwise = 20000

costoReparacion :: Auto -> Precio
costoReparacion  auto
    | ((==7).length.patente) auto  = 12500
    | ((==6).length.patente) auto && ((>= "DJ").take 2.patente) auto && ((<= "NB").take 2.patente) auto = (calculoPatental.patente) auto
    | otherwise = 15000

-- Punto 2a (auto peligroso)
-- Dado un auto, saber si es peligroso
-- Esta condicion se cumple cuando el desgaste
-- de la primera llanta es mayor a 0.5

autoPeligroso :: Auto -> Bool
autoPeligroso = (>0.5).head.desgasteLlantas

-- Punto 2b (revision auto)
-- Dado un auto, saber si necesita revision
-- Esta condicion se cumple cuando el ultimo
-- arreglo fue realizado en el año 2015 o antes

revisionAuto :: Auto -> Bool
revisionAuto = (<= 2015).anio.ultimoArreglo

-- Punto 3a (Personal tecnico encargado de reparaciones)
-- Alfa: hace que el auto regule a 2000 vueltas, salvo que
--       este a menos de 2000, en cuyo caso lo deja como esta.
-- Bravo: cambia todas las cubiertas dejandolas sin desgaste.
-- Charly: realiza las mismas actividades que Alfa y Bravo.

type RPM = Number

modificarRPM :: RPM -> RPM
modificarRPM rpm | 2000 > rpm = rpm
                 | otherwise = 2000

operadorAlfa :: Auto -> Auto
operadorAlfa auto = auto {
    rpm = (modificarRPM.rpm) auto 
}

operadorBravo :: Auto -> Auto
operadorBravo auto = auto {
    desgasteLlantas = [0, 0, 0, 0]
}

operadorCharly :: Auto -> Auto
operadorCharly auto = auto {
    rpm = (modificarRPM.rpm) auto,
    desgasteLlantas = [0, 0, 0, 0]
}

-- Punto 3b (Personal tecnico encargado de reparaciones)
-- Tango: le gusta decir que hizo muchas cosas pero en
--        realidad no hace ningun arreglo.
-- Zulu: revisa la temperatura del agua, la deja a 90 y 
--       hace lo mismo que Lima.
-- Lima: cambia las cubieras delanteras (dos primeras),
--       dejandolas sin desgaste. Las posteriores quedan igual.

operadorTango :: Auto -> Auto
operadorTango auto = auto

operadorZulu :: Auto -> Auto
operadorZulu auto = auto {
    temperaturaAgua = 90,
    desgasteLlantas = 0 : 0 : ((drop 2.desgasteLlantas) auto)
}

operadorLima :: Auto -> Auto
operadorLima auto = auto {
    desgasteLlantas = 0 : 0 : ((drop 2.desgasteLlantas) auto)
}