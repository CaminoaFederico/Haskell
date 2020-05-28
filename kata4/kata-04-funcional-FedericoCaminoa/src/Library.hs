module Library
  ( ambiciosas,
    personas,
    nombre
  ) where

import Data.Char (toLower)
import PdePreludat
import Data.List (isInfixOf)

data Persona = Persona {
  nombre :: String,
  suenios :: [String]
} deriving Show

type Gente = [Persona]
gogo :: Persona
gogo = Persona "gogo" ["ser presidente", "manejar una autobomba", "que mi viejo reconozca mis logros"]

daenerys :: Persona
daenerys = Persona "daenerys" ["saber lo que es bueno", "gobeRnar con misericordia", "que Tigre vuelva a primera"]

personas :: Gente
personas = [
  gogo,
  Persona "berta" ["ayudar a conseguir la paz mundial", "ser feliz"],
  Persona "khal" ["ser temido", "casarme con mi alma gemela"],
  daenerys
  ]

ambiciosas :: Gente -> Gente  --filtra a las personas ambiciosas
ambiciosas = filter esAmbiciosa 

esAmbiciosa :: Persona -> Bool  --busca las palabras presidente o gobernar en las listas de palabras para saber si una persona es ambiciosa
esAmbiciosa personaAmbiciosa = (any (elem "presidente") $ divideOraciones personaAmbiciosa) || (any (elem "gobernar") $ divideOraciones personaAmbiciosa) 

divideOraciones :: Persona -> [Suenios] --divide las oraciones formando listas de palabras
divideOraciones unaPersona = map words $ listaDeMinuscula unaPersona 

type Suenios = [String]
listaDeMinuscula :: Persona -> Suenios  --normaliza todas las letras dejandolas en minuscula
listaDeMinuscula persona = map (\unaPersona -> map toLower unaPersona) $ suenios persona 






