module Simpson where 
import PdePreludat


data Jugador = Jugador {
    nombre:: String, 
    padre :: String, 
    habilidad :: Habilidad
} deriving (Show)

data Habilidad = Habilidad {
    fuerzaJugador :: Number, 
    precisionJugador :: Number
} deriving (Show)

bart = Jugador "Bart" "Homero" (Habilidad 25 60)

todd = Jugador "Todd" "Ned" (Habilidad 15 80)

rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)


data Tiro = Tiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


type Palo = Habilidad -> Tiro 

putter :: Palo 
putter habilidad = Tiro {velocidad = 10, precision = precisionJugador habilidad *2 , altura =0}

madera habilidad = Tiro {velocidad = 100, altura = 5, precision = precisionJugador habilidad / 2}

-- hierros n habilidad = Tiro {velocidad = fuerzaJugador habilidad * 3}  
hierros n habilidad 
    | between 1 10 n = Tiro{velocidad = fuerzaJugador habilidad * n, precision = precisionJugador habilidad / n, altura = max 0 (n-3)}
    
-- para este palo, piden un criterio para la variable n, la cuál debe estar entre 1
-- 10, ya tenemos una función que se encarga de eso, ahora como metemos esa condición
-- en esta expresión. 

    -- (Habilidad -> Tiro), interpretamos a Palo como una función que devuelve Tiro dada una habilidad
golpe :: Palo -> Jugador -> Tiro
golpe t = t . habilidad  


tiroDetenido = Tiro {altura =0, velocidad = 0, precision =0} 

-- esta problemática se presenta como un enunciado en el cuál nos piden
-- además de comprobar una condición, modificar al sujeto... 

-- no habría problemas con el primer diseño, si es que no existiera el 
--punto 4 

-- Obstaculo -> Tiro -> Tiro 

condicionRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro 

vaAlRasDelSuelo = (0==) . altura 

condicionLaguna tiro = velocidad tiro > 80 && between 1 5 (altura tiro)

condicionHoyo tiro = between 5 20 (velocidad tiro) && precision tiro > 95

tiroRampita tiro = Tiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}
tiroLaguna largo tiro = Tiro {velocidad = velocidad tiro, precision = precision tiro, altura = altura tiro / largo}

tiroHoyo tiro = tiroDetenido

-- tunelConRampita tiro
--     | precision tiro > 90 = Tiro {velocidad = velocidad tiro * 2, precision = 100, altura = 0}
--     | otherwise = tiroDetenido

-- laguna largo tiro 
--     | velocidad tiro > 80 && between 1 5 (altura tiro) = Tiro {altura = altura tiro / largo, velocidad = velocidad tiro, precision = precision tiro }
--     | otherwise = tiroDetenido

-- hoyo tiro 
--     | between 5 20 (velocidad tiro) && precision tiro > 95 = Tiro {altura = 0, velocidad = 0, precision = 0}
    
-- estas funciones están mal, porque se encargan de varias cosas a la vez, primero 
-- la comprobación de la condición (diferentes, pero terminan teniendo el mismo comportamiento) , luego de tener que devolver otro tiro completamente
-- distinto, si por alguna razón necesitamos esto es parte de un proyecto grande
-- implicaría cambiar varias cosas de una función... 

-- se plantea una solución en la cuál exista una función de orden 
-- superior la cuál tiene todos los comportamientos 
-- repetidos (aplicación de condición y devolución del tiro)

-- las condiciones y devoluciones de tiro se abstraen en funciones 
-- aparte, logrando así la cohesividad

-- y son las funciones de tiros las que directamente implementan esta función de
-- orden superior 

superaTiro condicion transformacion tiro 
    | condicion tiro = transformacion tiro 
    | otherwise = tiroDetenido

rampita = superaTiro condicionRampita tiroRampita 
laguna largo  = superaTiro condicionLaguna (tiroLaguna largo)  
hoyo = superaTiro condicionHoyo tiroHoyo 


-- la aplicación de este tipo de funciones permite una mayor cohesión de funciones
-- sin embargo tiene como principal consecuencia el tener que armar varias funciones
-- para este caso (la de condición y la de transformación)



                    -- (Tiro -> Tiro)-> [Habilidad->Tiro]  
-- palosUtiles :: Jugador -> Obstaculo -> [Palo]
-- palosUtiles jugador =   . habilidad 

-- Si lo pensamos, nos piden operar con las habilidades de un jugador, en donde
-- según su habilidad, generamos los Palos 

-- 4: piden determinar que palos permiten pasar el obstaculo
-- dada la habilidad de una persona 

-- para poder pasar un obstaculo entonces primero debemos generar al tiro, 
-- con el palo 
    -- si ese palo pasa, lo agregamos a la lista, sino no y seguimos con el siguiente

-- Palo = (Habilidad -> Tiro) 
        -- a ese Tiro debemos comprobarlo con la condición si pasa o no

palos = [putter, madera, hierros 10]

-- palosUtiles :: Jugador -> (Tiro -> Bool) -> [Palo]
    -- 4 ) piden definir la función de arriba, dado un Jugador (su habilidad)
    -- y un obstaculo (criterio para superarlo) 
    -- decidir que palos le son útiles 

    -- como tal la función no se tiene que encargar de comprobar condiciones
    -- exactas para cada obstaculo
    -- o de armar listas (se supone que existen las funciones de orden)
    -- superior para eso

    -- el problema está en como aplicamos la condición con el palo de la lista
    -- de palos: [palo1, palo2, palo3]:  
        -- palo . habilidad 

palosUtiles jugador condicionObstaculo = filter (sirveParaSuperar jugador condicionObstaculo ) palos 

sirveParaSuperar jugador condicionObstaculo palo = condicionObstaculo palo habilidad jugador
-- condicion . palo . habilidad 

filtrado _ [] = []
filtrado condicion (x:xs) 
    | condicion x = x : filtrado condicion xs 
    | otherwise = filtrado condicion xs 