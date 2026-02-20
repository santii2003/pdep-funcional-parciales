module NumerosPositivos where 
import PdePreludat
    

-- 1 Números Positivos 

--Un desarrollador que dice ser cinturón negro en Haskell nos dice que el siguiente código 
-- es espectacular y no tiene fallas 

-- positivo :: Ord a => a => a 
-- positivo 
--    | x < 0 == True = False
--     | x > 0 == False = True 


-- Marcar errores que hacen que directamente no funcione correctamente
        -- El no devolver un valor booleano para denotar que el número x que recibimos
        -- es positivo es un error, ya que los guardas implican que debemos deolver valores 
        -- booleanos en el valor que retornan. 

        -- Además nunca se explicita el parámetro x en la función antes de abrir las guardas
-- Indicar cosas que si bien funcionan, se pueden hacer mejor 
        -- El x < 0 == True y su contraparte x > 0 == False es algo redundante, ya quq
        -- la expresión x < 0 ya devuelve de por si un valor booleano

-- Implementar una versión corregida 

positivo :: Number -> Bool
positivo = (>0)

-- 2 Costos de entrada
-- Esta función no tiene errores de funcionamiento y devuelve los valores correctos del precio
-- de la entrada según la edad de la persona

-- costoEntrada :: Number -> Number 
-- costoEntrada edad 
--     |  edad > 18 = 100 + 2 * (edad+1)
--     | edad > 60 = 100 + 2 * (edad/2 -20)
--     | otherwise = 100 + 2 * (18 - edad)

-- Hacer una versión que siga funcionando de la misma forma, pero mejor hecha
-- Justificar brevemente por qué la nueva versión es mejor

-- Esta versión de costo entrada tiene una clara problemática, el tener que encargarse de 
-- varias cosas a la vez, la cohesión de función se pierde
-- se encarga además de devolver el coste, de calcularlo y eso es un error
-- porque si después se quieren agregar 40 criterios más implicaría hacer una función muy larga

-- lo mejor sería delegar la responsabilidades ... ¿Cuáles?
    
-- o abstraer esto en un data? 

data CostoEntrada = CostoEntrada {
    criterio :: Number -> Bool, 
    costo :: Number -> Number        
} deriving (Show)


-- 3 Devuelve Algo 
    -- Funciones
    -- devuelveAlgo x y z w 
    --    | x y && x z = w y + z 
    --    | otherwise = z

    -- Esta función devuelve un number después de aplicar una expresión booleana 
    -- x :: a -> Bool, w :: Number -> Number 
    -- por lo tanto y, z :: Number  
devuelveAlgo :: (Number->Bool) -> Number -> Number -> (Number -> Number) -> Number
devuelveAlgo x y z w 
    | x y && x z = w y + z  
    | otherwise = z 

devuelveOtraCosa :: Number -> (Number->Bool) -> (Number->Number) -> [Number] -> Bool 
devuelveOtraCosa a b c = (>a) . length . filter b . map c 


-- 4 Sistemas de test 
-- devuelve una lista de números, por el resultado de should be
-- mayoresQueNAlAplicar :: ???? -> [Number]
-- además vemos que es recursiva e implementa un caso base, por el último
--test que debe dar []

-- el primer parámetro es un número 
-- el segundo es una función que es aplicada a un número y devuelve un número
-- el tercero es una lista


mayoresQueNAlAplicar :: Number -> (Number -> Number) -> [Number] -> [Number]

--caso base
mayoresQueNAlAplicar _ _ [] = []
mayoresQueNAlAplicar n f (x:xs)    
                                | f x > n = x : mayoresQueNAlAplicar n f xs  
                                | otherwise = mayoresQueNAlAplicar n f xs


--maximoSegun :: función que recibe una función de number a number
-- y una lista. Y devuelve el número mayor o máximo según esa función aplicada. 

maximoSegun :: Ord c => (a -> c) -> [a] -> c
maximoSegun f = maximum . map f 


minimoSegun :: Ord c => (a -> c) -> [a] -> c
minimoSegun f = minimum . map f 
-- ¿Porque esta expresión :: maximum . map f [...] da error en consola?

-- 5 Votación !!
-- 
data Mesa = Mesa {
    distrito :: String, 
    votos :: [Partido], 
    votosEmitidos :: Number, 
    cantidadVotosHabilitados :: Number 
} deriving (Show)

-- a menor sea el indice -> mayor presentismo
indiceAusentismo :: Mesa -> Number 
indiceAusentismo mesa = cantidadVotosHabilitados mesa - votosEmitidos mesa 

data Eleccion = Eleccion {
    listaMesas :: [Mesa], 
    listaPartidosParticipantes :: [Partido]
} deriving (Show)

data Partido = Partido {
    nombrePartido :: String
} deriving (Show)

-- a: partido ganador 
-- es por obvia razón que debo maniuplar la lista 

-- 

-- ganador = map (length. votos) . map listaMesas estaba mal 
--porque como tal el map listaMesas transformaba una lista de mesas 
-- en lista de listas... 

-- ganador debe hacer varias cosas en una sola expresión...
    -- calcular el partido ganador a través de los votos que obtienen de una
    --elección
        -- tengo problemas en interpretar [Mesa], ya que
        -- cada una tiene una [Partido] y nosotros debemos contar el partido 
        -- con mayor apariciones 



-- ganador :: Eleccion -> Partido 
-- ganador  = map votos . listaMesas 

--irregular :: Eleccion -> Bool 
irregular = any (\ v-> votosEmitidos v > cantidadVotosHabilitados v) . listaMesas

-- obtener mesa con el menor indice de ausentismo o mayor indice de presentismo

-- primero debemos transformar a cada mesa de la eleccion a 
-- un indice .:. Mesa -> Number

-- mayorIndice :: Eleccion -> Mesa
-- mayorIndice eleccion =  




--lista infinitas -> uso devuelta.... 
-- mismo problema que con música funcional -> problema en interpretación
-- de las listas....

-- irregular se quedaría colgado en caso de no encontrar irregularidades,
-- porque la función de   



alguno _ [] = False
alguno criterio (x:xs) = criterio x || alguno criterio xs


todos _ [] = True 
todos criterio (x:xs) = criterio x && todos criterio xs 