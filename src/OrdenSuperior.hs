module OrdenSuperior where
import PdePreludat


-- Si una función f recibe en algunos de sus argumentos una función entonces f es una función de orden superior. Veamos un ejemplo. 
-- Si quiero aplicar a un número n una función determinada podría hacer.. 
aplicar :: (a->b)->a->b
aplicar f  = f 
-- Por ejemplo, le paso como argumento una función aplicada parcialmente: (+3), (4*). 
-- Main> aplicar (+ 3) 2 
-- 5 
-- Main> aplicar (4 *) 3 
-- 12 


-- Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos devuelve True si existe algún elemento de la tupla que haga verdadera la función. 
-- Main> existsAny even (1,3,5) 
-- False 
existsAny :: (a-> Bool) -> (a,a,a) -> Bool 
existsAny f (x,y,z) = f x || f y || f z

-- Main> existsAny even (1,4,7) 
-- True 
-- porque even 4 da True 

-- Main> existsAny (0>) (1,-3,7) 
-- True 
-- porque even -3 es negativo


-- Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto. P.ej. 

mejor :: (Number -> Number) -> (Number -> Number) -> Number -> Number 
mejor f1 f2 n = max (f1 n) (f2 n)

-- Main> mejor cuadrado triple 1  d
-- 3 
-- (pues triple 1 = 3 > 1 = cuadrado 1) 

-- Main> mejor cuadrado triple 5 
-- 25 
-- (pues cuadrado 5 = 25 > 15 = triple 5) 
-- Nota: No olvidar la función max. 




-- Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que resulta de aplicar la función a los elementos del par. P.ej. 
-- Main> aplicarPar doble (3,12) 
-- (6,24) 

-- Main> aplicarPar even (3,12) 
-- (False, True) 

-- Main> aplicarPar (even . doble) (3,12) 
-- (True, True) 

aplicarPar :: (Number-> a) -> (Number, Number) -> (a, a)
aplicarPar f (x,y) = (f x, f y)


-- Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par ordenado que es el resultado de aplicar las dos funciones al valor. P.ej. 
-- Main> parDeFns even doble 12 
-- (True, 24)

parDeFns :: (a->b) -> (a->d) -> a -> (b,d)
parDeFns f1 f2 valor = (f1 valor, f2 valor)


-- Orden Superior + Listas
-- Además existen funciones de orden superior predefinidas que nos permiten trabajar con listas. Por ej: 
-- Si quiero filtrar todos los elementos de una lista determinada que cumplen una determinada condición puedo utilizar filter. 
-- paresEntre n1 n2 = filter even [n1..n2] 

-- Otro Ejemplo de funciones de orden superior predefinidas que se utiliza mucho es el map Si quiero transformar una lista de elementos, puedo hacer: 
-- sumarN n lista = map (+n) lista 
-- Suma n a cada elemento de la lista. 
-- sumarElDobleDeN n lista = map (+ (doble n)) lista 
-- Aplica el doble a cada elemento de la lista. 

-- Otras funciones de orden superior: 
-- all even [2,48,14] = True -- Indica si todos los elementos de una lista cumplen una condición. 
-- all even [2,49,14] = False 
-- any even [2,48,14] = True -- Indica si algunos de los elementos de una lista cumplen una condición



--1
esMultiploDeAlguno :: Number -> [Number] -> Bool 
esMultiploDeAlguno n = any (esMultiplo n) 

esMultiplo :: Number -> Number -> Bool 
esMultiplo n m = mod n m == 0 


-- 2) 
promedios :: [[Number]] -> [Number]
promedios = map promedio 

promedio :: [Number] -> Number
promedio numeros = sum numeros / length numeros   


--3) 
promediosSinAplazos :: [[Number]] -> [Number]
promediosSinAplazos = map promedioSinAplazo

promedioSinAplazo :: [Number] -> Number
promedioSinAplazo = promedio . filter (>4)
 
 --4) 
mejoresNotas :: [[Number]] -> [Number]
mejoresNotas = map maximum 

--5) 
aprobo :: [Number] -> Bool 
aprobo = all (>=6) 

--minimum 
aproboV2 :: [Number] -> Bool 
aproboV2 = (>=6) . minimum  

--6) 
aprobaron :: [[Number]] -> [[Number]]
aprobaron = filter aprobo 


--7)
divisores :: Number -> [Number]
divisores n = undefined

-- Ayuda: para calcular divisores n alcanza con revisar los números entre 1 y n
-- Seguramente implica funciones de generación de listas infinitas... 

exists :: (a->Bool) -> [a] -> Bool
exists f = any f

hayAlgunNegativo :: (Number -> Number) -> [Number] -> Bool 
hayAlgunNegativo f = exists (<0) . map f 

-- aplicarFunciones :: [a->a] -> a -> [a]
aplicarFunciones [] _ = []
aplicarFunciones (f:fs) valor = f valor : aplicarFunciones fs valor 
 
-- Main> aplicarFunciones[(*4),even,abs] 8 
-- da error. ¿Por qué? 
-- Porque even va de (Number -> Bool), y esto rompe con la igualdad del tipado
-- de todas las funciones de las listas: 
    -- las funciones van de Number -> Number, si meto un con otro tipado me rompe el programa

sumaF funciones  = sum . aplicarFunciones funciones  

-- 12) 
subirHabilidad :: Number -> [Number] -> [Number]
subirHabilidad n = map (min 12 . (+2))

flimitada :: (Number -> Number) -> Number ->  Number 
flimitada f = max 0 . min 12 . f 

cambiarHabilidad f = map (flimitada f)  

-- usar cambiarHabilidad para llevar a 4 los que tenían menos de 4, dejando como estaban al resto


-- final 
data VideoJuego = Videojuego {
    titulo :: String, 
    desarrolladora :: String, 
    generos :: [String], 
    lanzamiento :: Number
} deriving (Show)

esDesarrolladoPor dev = (==dev) . desarrolladora 

perteneceAGenero genero = elem genero . generos 

esReciente = (>2015) . lanzamiento

type Preferencia = VideoJuego -> Bool 
type Jugador = [Preferencia]

juan :: Jugador
juan = [esDesarrolladoPor "nintendo" , perteneceAGenero "plataformas"]
maria = [esReciente]

pedro = [esDesarrolladoPor "rockstar", perteneceAGenero "mundoAbierto"]

-- jugadores = [juan, maria, pedro]

preferidosDelComite :: [Jugador] -> [VideoJuego] -> [VideoJuego]
preferidosDelComite  jugadores = filter (esValoradoPositivamente jugadores) 

-- está mal la relación, porque pensé la función de orden superior muy compleja, 
-- una solución simple era planteralo como: 
    -- Cada videojuego va a pertenecer a la lista filtrada, si es del agrado de todos los jugadores


-- valoradoPositivamente :: [Preferencia] -> VideoJuego -> Bool 
-- valoradoPositivamente [] _ = True
-- valoradoPositivamente (f:fs) videojuego 
--     | f videojuego = valoradoPositivamente fs videojuego
--     | otherwise = False 


esValoradoPositivamente :: [Jugador] -> VideoJuego -> Bool 
esValoradoPositivamente jugadores videojuego = all (valuaPositivamente videojuego) jugadores 

-- all ( any (\preferencia -> preferencia videojuego) ) jugadores 

    -- adentro del all, estamos diciendo : 
    -- cada (any) preferencia (de la lista de preferencias de un jugador, sea
    -- comprobado con el videojuego, si es asi para todos )

valuaPositivamente ::  VideoJuego -> [Preferencia] -> Bool
valuaPositivamente _ [] = True
valuaPositivamente videojuego (f:fs)  
    | f videojuego = valuaPositivamente videojuego fs 
    | otherwise = False 
 
data Alimento = Comida {
    nombre :: String,
    calorias :: Number, 
    nutrientes :: [String]
}deriving (Show)

ana :: [Alimento -> Bool]
-- ana = [esBajoEnCalorias, tieneProteinas]

esBajoEnCalorias :: Alimento -> Bool 
esBajoEnCalorias comida = calorias comida < 100

tieneProteinas :: Alimento -> Bool 
tieneProteinas  = elem "proteinas" . nutrientes 

tieneNutriente :: String -> Alimento -> Bool 
tieneNutriente nutriente = elem nutriente . nutrientes 


ana = [esBajoEnCalorias, tieneNutriente "proteina"]

--3) Se cuenta con una función que permite establecer, dada una lista de alimentos, cuales 
-- elegiría la persona según sus requerimientos, o preferencias.
-- Asumiendo que elige un alimento que cumpla al menos 1

-- alimentosElegidos :: [Alimento -> Bool] -> [Alimento] -> [Alimento]
-- alimentosElegidos requisitos alimentos = filter (f requisitos) alimentos

-- f:: [Alimento -> Bool] -> Alimento -> Bool 
-- f requisitos alimento = any (\requisito -> requisito alimento) requisitos

-- a) La solución parece funcionar correctamente. alimentosElegidos delega en filter
-- la condición para que elegir a un alimento, mediante f 

--como la condición era que el alimento cumpla al menos con 1 
-- requisito del cliente, entonces:>
-- any (\requisito -> requisito alimento) requisitos

--b && c)
-- la solución no es poco declarativa, debido a que utiliza funciones de orden
-- superior para dejar en claro de que se encarga cada uno, reduciendo código

-- la solución es poco expresiva, puesto al nombre de f, no se puede intuir que es lo que hace solo
-- leyendo el f 

-- un cambio de nombre posible puede ser: cumpleAlgunRequisito 

--d) Verdadero, el filter evalua toda la lista para tratar de filtrar, quedando en un bucle infinito
-- puesto que no es como otras funciones, de tipo lazy... 
-- filter necesita evaluar si o si a todos los elementos de la lista, por 
-- eso queda en bucle infinito, lo mismo map, all,

-- funciones como any, o de ese estilo solo necesitan 1 elemento,
-- por lo tanto van a terminar, ya que las evidentemente cuándo encuentre a uno corta, sino
-- si entra en bucle infinito cuándo no puede cumplir con el elemento

-- funciones como head, !!n si terminan, bajo cualquier caso, ya que 
-- son de evaluación lazy, por lo tanto solo le van a ir pidiendo a la lista, un determinado
-- elemento hasta llegar a la posición buscada (head = 0) o !!n



desplazamiento :: [a] -> Number -> a
desplazamiento [x] _ = x 
desplazamiento (x:xs) n = desplazamiento xs (n-1) 

    -- head (x:_) = x 

