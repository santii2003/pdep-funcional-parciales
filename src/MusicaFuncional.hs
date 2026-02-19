module MusicaFuncional where 
import PdePreludat 

type Cancion = [Nota]

data Nota = Nota {
    tono :: Number, -- Frecuencia medida en Hz
    volumen :: Number, -- Volumen de reproducción medida en Db
    duracion :: Number -- Tiempo de reproduccón medido en segundos 
} deriving (Eq, Show)


---------------------------------------------

-- Una nota estándar, audible y tranquila
la440 :: Nota
la440 = Nota 440 60 2.0

-- Nota grave y MUY fuerte (Molesta por tono < 250 y vol > 85)
trueno :: Nota
trueno = Nota 100 95 3.5

-- Nota aguda y fuerte (Molesta por tono >= 250 y vol > 55)
chirrido :: Nota
chirrido = Nota 3000 70 1.0

-- Nota inaudible por baja frecuencia (Suma a silencioTotal)
subsonica :: Nota
subsonica = Nota 15 5 2.0

-- Nota inaudible por alta frecuencia y poco volumen
ultrasonido :: Nota
ultrasonido = Nota 25000 5 1.5

-- Nota inaudible pero MUY CORTA (Para probar sinInterrupciones)
ruiditoCorto :: Nota
ruiditoCorto = Nota 10 5 0.05

---------------------------------------------------------
-- CANCIONES (LISTAS DE NOTAS)
---------------------------------------------------------

-- 1. Una canción que tiene 2 segundos de "silencio" (inaudibles)
-- silencioTotal debería dar 2.0
cancionConSilencios :: Cancion
cancionConSilencios = [la440, subsonica, chirrido]

-- 2. Una canción que debería devolver True en sinInterrupciones
-- Porque aunque tiene una nota inaudible, esta dura menos de 0.1s
cancionFluida :: Cancion
cancionFluida = [la440, ruiditoCorto, la440]

-- 3. Una canción para probar el peorMomento
-- peorMomento debería dar 95.0 (el volumen del trueno)
cancionPesadilla :: Cancion
cancionPesadilla = [trueno, chirrido, la440]

-- 4. Una canción que rompe sinInterrupciones
-- Porque tiene una nota inaudible que dura más de 0.1s
cancionInterrumpida :: Cancion
cancionInterrumpida = [la440, ultrasonido, la440]

-- Funciones auxiliares

cambiarVolumen :: (Number -> Number) -> Nota -> Nota 
-- Dada una función transformación y una nota retorna una copia de la nota con el  
-- volumen igual al resultado de aplicar la transformación a su volumen actual 
cambiarVolumen delta nota = nota {volumen = delta (volumen nota)}

cambiarTono :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la nota 
-- con el tono igual al resultado de aplicar la transformación a su tono actual 
cambiarTono delta nota = nota {tono = delta (tono nota)}

cambiarDuracion :: (Number -> Number) -> Nota -> Nota
-- Dada una función transformación y una nota retorna una copia de la nota 
-- con la duración igual al resultado de aplicar la transformación a su duración actual 
cambiarDuracion delta nota = nota {duracion = delta (duracion nota)}

promedio :: [Number] -> Number
--Dada una lista de números retorna el valor promedio
promedio lista = sum lista / length lista

--Punto 1 
--a) 
entreNumeros :: Number -> Number -> Number -> Bool  
entreNumeros min max nro = nro >= min && nro <= max

esAudible :: Nota -> Bool 
esAudible nota = (entreNumeros 20 20000 . tono) nota && volumen nota > 10

--b) 
esMolesta :: Nota -> Bool 
esMolesta nota  | tono nota < 250 && volumen nota > 85 = True
                | tono nota >= 250 && volumen nota > 55 = True
                |  not . esAudible $ nota =             False
                | otherwise = False 

--b está mal bajo los parámetros de funciones de orden superior, hay 
-- muchas guardas

-- la función solo está definida por partes.. 
-- si el volumen nota > 85 que se compare 



--2) 
--a)
silencioTotal :: Cancion -> Number 
silencioTotal = sum . map duracion . filter (not . esAudible)

--b) 
sinInterrupciones :: Cancion -> Bool 
sinInterrupciones = all esAudible . filter ((>0.1) . duracion)

--c) 
peorMomento :: Cancion -> Number 
peorMomento = maximum . map volumen . filter esMolesta 

--3) 
type Filtro = Cancion -> Cancion

--a
transponer :: Number -> Filtro 
transponer escalar = map  (cambiarTono  (*escalar))


acotarVolumen :: Number -> Number -> Filtro 
acotarVolumen maximo minimo = map (cambiarVolumen (min maximo . max minimo))

-- ¿Cómo funciona?
-- Si una Nota tiene un volumen mayor al máximo -> se acota el volumen al límite 
--mayor 

    -- comparamos primero minimo con el volumen actual -> mayor entonces volumen actual
    -- comparamos ahora cuál es el min entre el maximo permitdo y el actual
    -- como el maximo es el actual, entonces el min da como resultado el máximo 
    --permitido, logrando así que cambiemos el volumen al min entre 
    -- maximoPermitido y volumenExcedido

    

--c
--Este filtro modifica cada nota de una canción seteando su volumen
-- al volumen promedio de la canción original 



-- normalizar cancion = map (cambiarVolumen (\_ -> promedio (map volumen cancion))) cancion--    


normalizar :: Filtro
normalizar cancion = map (setearVolumen (volumenPromedio cancion)) cancion

-- acá en esta solución ideada, darío delega la responsabilidad 
-- de cambiarVolumen y calcular el promedio en otras dos funciones
-- haciendo que normalizar se encargue esencialmente de aplicar función a las
-- notas de una canción


setearVolumen :: Number -> Nota -> Nota
setearVolumen nuevoValor nota = nota{volumen = nuevoValor}

volumenPromedio :: Cancion -> Number
volumenPromedio = promedio . map volumen




-- dada la función 

-- f g [] y z = g y z 
-- f g (x:xs) y z = g(x y) z || f g xs y z 

--definir su tipo
-- sabemos que el caso de arriba es el caso base de una función probablemente recursiva,
-- llamada f
-- si f recibe una lista vacía entonces se aplica g y z
-- sino, se utiliza el parámetro x junto con y, junto z: 
    -- g (x y) z
-- g parece recibir siempre dos parámetros, no listas, ya que utiliza x directamente 
-- el problema está en la complejidad de g, no de los otros parámetros 

-- leyendo el enunciado de abajo, podemos asegurar que la lista que recibimos es de funciones
-- por lo tanto tenemos que f :: ? -> [a->a]
-- y que g recibe dos parámetros del mismo tipo y los compara. para el caso de 
-- g (x y) tenemos que como x es una función, es aplicada parcialmente con y es comparada con z 

f :: (a->a->Bool) -> [a->a] -> a -> (a -> Bool)
f g [] y z = g y z 
f g (x:xs) y z = g (x y) z || f g xs y z 

-- utilizar f para definir la función ....
infringeCopyright :: [Filtro] -> Cancion -> Cancion -> Bool 
infringeCopyright filtros cancionOriginal cancionSospechosa
    | cancionOriginal == cancionSospechosa = True 
    | otherwise = f (\a b -> a == b) filtros cancionOriginal cancionSospechosa 


-- se puede mejorar f en términos de Declaratividad y Expresividad al cambiar primero el nombre de la función 
-- y de sus parámetros, ya que g es una función comparadora y la lista es una lista 
-- de funciones 


tunear :: [Filtro] -> Cancion -> Cancion 
-- dada una lista de filtro, una cancion  retorne una nueva canción que es resultado de aplicar
-- todos los filtros a la cancion y que después de ahí la normalice 

-- normalizar . ???????????? 

-- habíamos visto que si piden :: Lista de funciones aplicada a un elemento entonces
-- foldr ($) parametro lista
-- al tener que pasar primero los filtros para que la función sea point free 
-- entonces realizo un flip a la expresión foldr ($)

tunear filtros = normalizar . flip  (foldr ($)) filtros



-- Canción simple para caso base
cancionTest1 :: Cancion
cancionTest1 = [la440, la440, la440]

-- Canción transpuesta de cancionTest1
cancionTest1Transpuesta :: Cancion
cancionTest1Transpuesta = transponer 2 cancionTest1

-- Canción normalizada de cancionTest1
cancionTest1Normalizada :: Cancion
cancionTest1Normalizada = normalizar cancionTest1

-- Canción con filtros combinados
cancionTest1Combo :: Cancion
cancionTest1Combo = normalizar (transponer 1.5 cancionTest1)

-- Canción corta para pruebas rápidas
cancionCorta :: Cancion
cancionCorta = [la440, chirrido]

-- Canción corta modificada
cancionCortaModificada :: Cancion
cancionCortaModificada = transponer 0.5 cancionCorta

--Canción totalmente diferente
cancionDiferente :: Cancion
cancionDiferente = [trueno, subsonica, ultrasonido]