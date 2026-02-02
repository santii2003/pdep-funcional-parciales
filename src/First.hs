module First where
import PdePreludat


data Persona = Persona {
    nombre :: String, 
    jubilacion :: Number
} deriving (Show)

juan = Persona "Juan" 20000
ana = Persona "Ana" 45000
pedro = Persona "Pedro" 15000
santiago = Persona "Santiago" 150000

personas = [juan, ana, pedro, santiago]

-- Ahora vamos con el ejercicio 3a
-- aplicarTransformacionAListaFiltrada (>3) +1) [1,2,3,4,5]

-- Ejercicio 3b y 3c 
esMayorACinco = (>5)
noTieneNombreLargo = (not . esMayorACinco . length . nombre)

subirJubilacion persona = persona {jubilacion = jubilacion persona + 300}

-- > aplicarTransformacionAListaFiltrada noTieneNombreLargo subirJubilacion personas

subirJubilacionSiElNombreEsCorto = aplicarTransformacionAListaFiltrada noTieneNombreLargo subirJubilacion 

-- Ejercicio 3D
-- > aplicarTransformacionAListaFiltrada (>3) (\n->n+3) [1,2,3,4,5,6,7,8,9,10]


aplicarTransformacionAListaFiltrada :: (a -> Bool) -> (a -> a) -> ( [a] -> [a] )
aplicarTransformacionAListaFiltrada criterio transformacion lista = map transformacion (filter criterio lista) ++ filter (not . criterio) lista

-- Ejercicio 4 - Implementación recursiva 

aplicarTransformacionAListaFiltradaRecursiva:: (a -> Bool) -> (a -> a) -> [a] -> [a]
aplicarTransformacionAListaFiltradaRecursiva _ _ [] = []
aplicarTransformacionAListaFiltradaRecursiva criterio transformacion (x:xs)
    | criterio x = transformacion x : aplicarTransformacionAListaFiltradaRecursiva criterio transformacion xs
    | otherwise  = x : aplicarTransformacionAListaFiltradaRecursiva criterio transformacion xs

maxBetweenThree :: Number -> Number -> (Number -> Number)
maxBetweenThree n1 n2 = max (max n1 n2)

-- Ejercicio 5, lista infinita como tercer parámetro. Se puede usar contexto con otras funciones

-- Ejercicio 5A que devuelva 6
-- (!!5) (aplicarTransformacionAListaFiltradaRecursiva (<=6) (+0) [0..] )

-- Ejercicio 5B que devuelva una lista infinita
-- aplicarTransformacionAListaFiltrada (>5) (+0) [5..]

-- Ejercico 5C
-- aplicarTransformacionAListaFiltrada (<5) (+1) [0..], se queda colgado después de procesar el 4, ya que tiene que filtrar a todos los que no cumplen, siendo estos 
-- una lista infinita