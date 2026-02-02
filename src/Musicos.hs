module Musicos where 
import PdePreludat
import Data.List (find, sort)

data Musico = Musico {
    nombre :: String, 
    grado :: Number, 
    instrumentoFavorito :: String,
    -- lista historial de actuaciones + cantidad de publico asistente 
   historialActuaciones :: [Actuacion] 
}deriving (Show)

data Actuacion = Actuacion {
    fecha :: (Number, Number, Number), 
    publicoAsistente :: Number
} deriving (Show)

actuacionColon = Actuacion (30,12,2006) 3000
actuacionIsidro = Actuacion (20,11, 2007) 6001
actuacionMiramar = Actuacion (10,08,2010) 3501
actuacionMarDelPlata = Actuacion (10, 07, 2005) 10000

mariana = Musico "Mariana" 9 "violin" [actuacionMiramar, actuacionColon, actuacionIsidro]
maria = Musico "Maria" 8 "oboe" [actuacionColon, actuacionIsidro]

santiago = Musico "Santiago" 5 "oboe" [actuacionMarDelPlata] 

-- Punto 1  Informer 


-- muchasActuaciones :: Musico -> Bool  
muchasActuaciones :: Musico -> Bool
muchasActuaciones = any ((> 5000) . publicoAsistente) . historialActuaciones
 

cuantasActuaciones :: Musico -> Number
cuantasActuaciones = length . historialActuaciones

-- Punto 2 -- Radioactivity 
instrumentosSumadores :: [String]
instrumentosSumadores = ["oboe", "fagot", "cuello"]

-- sumarExperiencia musico = musico {grado = grado musico +1}

cambiarInstrumento :: Musico -> String -> Musico
cambiarInstrumento musico nuevoInstrumento = musico {instrumentoFavorito = nuevoInstrumento}

tieneAlgunInstrumentoSumador :: Musico -> Bool
tieneAlgunInstrumentoSumador  = flip (elem . instrumentoFavorito ) instrumentosSumadores 

-- hacer esto en con type...
incrementarExperiencia :: Musico -> Musico 
incrementarExperiencia musico | grado musico <= 9 = musico {grado = grado musico +1 }
                              | otherwise = musico    

sumarExperiencia ::  Musico -> Musico
sumarExperiencia musico     | tieneAlgunInstrumentoSumador musico = incrementarExperiencia musico
                            | otherwise = musico 

-- tocarInstrumento :: String -> (Musico -> Musico)
-- tocarInstrumento instrumentoNuevo = (flip (cambiarInstrumento) instrumentoNuevo . sumarExperiencia)

tocarInstrumento :: String -> (Musico -> Musico)
tocarInstrumento instrumentoNuevo =  sumarExperiencia . flip cambiarInstrumento instrumentoNuevo 

cantar :: Musico -> Musico
cantar musico = musico {nombre = nombre musico++"LaLaLa"}

-- agregarPresentacion :: Musico -> Actuacion -> Musico
-- agregarPresentacion musico presentacion =  musico {historialActuaciones = (flip (:) . historialActuaciones) musico presentacion  }
--la versión de arriba es solo util cuándo nos piden agregar al principio, estructuras del tipo pila 


agregarPresentacion :: Musico -> Actuacion -> Musico
agregarPresentacion musico presentacion = musico {historialActuaciones =  historialActuaciones musico ++ [presentacion] }

-- hacerPresentacion presentacion  = (flip (agregarPresentacion) presentacion . sumarExperiencia)

hacerPresentacion :: (Number, Number, Number) -> Number -> Musico -> Musico
hacerPresentacion fecha publicoAsistente = flip (agregarPresentacion)  Actuacion{fecha = fecha, publicoAsistente = publicoAsistente} . sumarExperiencia . incrementarExperiencia

pensar :: Musico -> Musico 
pensar musico = musico 

-- tocarInstrumento "fagot" musico
-- cantar musico

-- Punto 3 
--

muchaExperiencia :: [Musico -> Musico] -> Musico -> Musico 
muchaExperiencia lista musico = foldr ($) musico lista 

-- transformacion :: (a->b) -> [a] -> [b]
-- transformacion _ [] = []
-- transformacion f (x:xs) = f x : transformacion f xs

--Punto 4   

obsesionadoConLaMayor :: Musico -> Bool 
obsesionadoConLaMayor = recorridoObsesion . map publicoAsistente . historialActuaciones  
                            

recorridoObsesion :: [Number] -> Bool 
recorridoObsesion [] = True 
recorridoObsesion[x] = odd x 
recorridoObsesion (x:y:xs) = odd x && even y == recorridoObsesion xs

