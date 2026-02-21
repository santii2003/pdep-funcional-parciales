module Carreras where
import PdePreludat 

data Auto = Auto {
    color :: String, 
    velocidad :: Number, 
    distancia :: Number
} deriving (Show)


-- De la carrera sólo nos interesa el estado actual de los autos que están participando, lo cual nos permitirá analizar 
--cómo viene cada uno, y posteriormente procesar aquellos eventos que se den en la carrera para determinar el resultado de la misma.

type Carrera = [Auto]

-- ¿Cuál es el estado actual de una carrera?

--a 
-- Saber si un auto está cerca de otro 
-- auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10

-- a partir del map distancia, convierto [Auto1, Auto2, ... AutoN] -> [DistanciaRecorrida1, DistanciaRecorrida2, ... DistanciaRecorridaN]
-- estaCerca :: Auto -> Auto -> Carrera -> Bool 

auto1 = Auto "rojo" 100 10
auto2 = Auto "azul" 154 19
auto3 = Auto "rojo" 133 8
 
auto4 = Auto "verde" 144 50
carrera1 = [auto1, auto2, auto4]


estaCerca :: Auto -> Auto -> [Auto] -> Bool
estaCerca auto otroAuto = (noSonIguales auto otroAuto &&) . (<10) . distanciaEntreNumeros . map distancia . filter (\a -> color a == color auto || color a == color otroAuto) 



noSonIguales auto otroAuto = color auto /= color otroAuto    


distanciaEntreNumeros :: [Number] -> Number
distanciaEntreNumeros  = abs . foldl1 (-)

--hace ruido la solución en no comprobar si el auto pasado por parametro pertenece a carrera 

--por ejemplo, si [10, 20] 

autoVaTranquilo  :: Auto -> Carrera -> Bool 
autoVaTranquilo auto carrera = estaPrimero auto carrera && sinRivalesCerca auto carrera 


-- si pensamos desde el inicio esta función, dado un auto y una lista de autos, no debe tener a ninguno cerca
-- entonces: para cada auto perteneneciente a la lista de autos, se cumple que -> no estaCerca auto1 autoLista carrera
sinRivalesCerca auto carrera = all (\a -> not (estaCerca auto a carrera)) carrera

--No tenerle miedo a la aplicación directa, a veces es necesario no tener que evitar siempre la notación point free

estaPrimero :: Auto -> [Auto] -> Bool
estaPrimero auto  = (distancia auto==) . maximum . map distancia 
-- la primera parte del auto más rápido, implica hacer un máximo, según ese criterio 
-- autoX [autoX, autoY, autoZ]

-- Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando.
-- ¿Cómo sé que auto me va ganando: -> distancia auto < distancia otroAuto ? 

puesto :: Auto -> [Auto] -> Number 
puesto auto = (+1) . length . filter (\a -> distancia a > distancia auto)

recorrer ::  Number -> Auto -> Auto
recorrer  tiempo auto= alterarRecorrido (+ (velocidad auto * tiempo)) auto 
-- modificarRecorrido auto tiempo = auto{distancia = distancia auto + velocidad auto * tiempo}

alterarRecorrido :: (Number -> Number) -> Auto -> Auto
alterarRecorrido modificador auto = auto{distancia = modificador (distancia auto)}


alterarVelocidad :: (Number -> Number) -> Auto -> Auto 
alterarVelocidad modificador auto = auto{velocidad =  modificador (velocidad auto)  }

bajarVelocidad :: Number -> Auto -> Auto
bajarVelocidad cantidad  = alterarVelocidad (\ v -> max 0 (v - cantidad))  

-- porque esta solución es la ideal? 
    -- sabemos que \v hace referencia al número que se recibe de la función     
    -- (Number -> Number) por lo tanto si queremos restar, solo utilizamos la resta 
    -- directa con la cantidad. Esto porque el orden importa, sino sin la utilización de 
    -- lambdas no se puede lograr. 


asignarVelocidad cantidad = alterarVelocidad (\v -> id cantidad )
-- la aplicación de const también puede servir, puesto que const entre el primer párametro
-- y el segundo se queda con el primero .:. 
-- modificador = const cantidad :: Number -> Number
    -- const cantidad (velocidad auto) :: Number
    -- como el primer parametro es cantidad, y el segundo la velocidad anterior del auto, 
    -- se elije el primer parámetro... cantidad 

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- autos -> pueden tener poderes especiales, 
--un poder sería la capacidad de modificar un auto. 

--  terrermoto -- luego de usar este poder, los autos que están cerca del que 
-- gatillo el power up bajan su velocidad en 50 

-- debemos encargarnos de aplicar el power up y bajar la (bajar la velocidad en 50 -> lo tengo esto)

terremoto :: Auto -> [Auto] -> [Auto]
terremoto auto = afectarALosQueCumplen (estaCercaV2 auto) (bajarVelocidad 50) 


estaCercaV2 :: Auto -> Auto -> Bool 
estaCercaV2 auto otroAuto = (<10)  (abs (distancia auto - distancia otroAuto))
-- aplicación directa -> permite inferir que (abs (distancia auto - distancia otroAuto)) es 
-- de tipo :: Number, y como (<10) :: Number -> Bool, encaja perfectamente ! 


-- miguelitos :: Number -> Auto -> [Auto] -> [Auto]
-- miguelitos cantidad auto carrera = afectarALosQueCumplen (??????) (bajarVelocidad cantidad)


-- (????) :: a-> Bool
-- la lista que tenemos es de autos .:. Auto -> Bool
-- realizando una función auxiliar, de tipo Auto -> Auto -> Bool 
-- la cuál nos indique que dados dos autos, comparar su puesto en la carrera 
-- sin embargo no tenemos el parámetro de la carrera, entonces otra opción puede ser

-- ¿Uso de lambda?

jetPack :: Number -> Auto -> [Auto] -> [Auto]
jetPack tiempo auto = afectarALosQueCumplen (not . noSonIguales auto) (alterarVelocidad (*2) . recorrer tiempo)

-- f :: (Number -> Bool ) ->[ (a, [Number]) ]  -> Bool 
-- f h = (>15) . sum . filter h . head . map snd 
--                                     --  map :: (a->b) -> [a] -> [b] ,snd :: (a,b) -> b
--                                     --


-- Práctica: algoritmo repetido

data Alimento = Alimento {
    nombre :: String, 
    precio :: Number, 
    calorias :: Number 
} deriving (Show)

-- a) ordenarAlfabeticamente:
    -- Para cada alimento a ordenar, lo ubico arriba del primero con mayor NOMBRE que ya haya ordenado

--b) ordenarPorPrecio 
    -- Para cada alimento a ordenar, lo ubico arriba del primero con mayor PRECIO que ya haya ordenado

--c) ordenarPorCalorias
    -- Para cada alimento a ordenar, lo ubico arriba del primero con mayor CALORÍAS que ya haya ordenado

-- lo único que no se repite es lo que está en MAYÚSCULAS -> Función de orden superior, que abstraiga como hacer lo demás, de lo que no se repite simplemente 
-- lo dejamos para más adelante

-- ordenar _ [] = []
-- ordenar criterio (x:xs) 
--     | criterio x (head xs) = x : ordenar criterio xs 
--     | otherwise = ordenar criterio xs ++ [x]
