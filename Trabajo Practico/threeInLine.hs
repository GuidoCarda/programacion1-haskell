import System.IO

data Ficha = Cruz | Circ | Empty deriving (Eq)


instance Show Ficha where
  show Cruz = "X"
  show Circ = "O"
  show Empty = "_"

type Fila = [Ficha]
type Tablero = [Fila]


{-
agregarCol 0 = []
agregarCol cols = Empty : agregarCol cols-1
-}


{-
threeline = do putStrLn "Ingrese la cantidad de filas"
               filas <- getLine
               putStrLn "Ingrese la cantidad de columnas"
               cols <- getLine
               print (agregarFilas ((read filas)::Int) ((read cols)::Int))
-}



-- threeline = do putStrLn "Ingrese la cantidad de filas"
--                filas <- getLine
--                putStrLn "Ingrese la cantidad de columnas"
--                cols <- getLine
--                impTab (agregarFilas ((read filas)::Int) ((read cols)::Int))


-- Creo un data type jugador del tipo registro que guarde el nombre de cada jugador y si esta jugando o no
-- este bool lo uso de bandera para ir intercambiando entre jugador 1 y 2
data Jugador = Jugador { nombre::String, estaJugando::Bool, esGanador::Bool} deriving Show

threeline = do {- putStrLn "Ingrese la cantidad de filas"
               filas <- getLine
               putStrLn "Ingrese la cantidad de columnas"
               cols <- getLine
               impTab (agregarFilas ((read filas)::Int) ((read cols)::Int)) -}
               --putStrLn "Ingrese nombre del jugador 1"
               --nombre1 <- getLine
               --putStrLn "Ingrese nombre del jugador 2"
               --nombre2 <- getLine
               let jugador1 = Jugador { nombre="joanco", estaJugando=True, esGanador=False}
               let jugador2 = Jugador { nombre="dogui", estaJugando=False, esGanador=False}
               print (nombre jugador1 ++ " " ++ mostrarEstaJugando (estaJugando jugador1))
               print (nombre jugador2 ++ " " ++ mostrarEstaJugando (estaJugando jugador2))
               --let jugador1 = Jugador { nombre=nombre1, estaJugando= False, esGanador=False, fichas=3} --Como los valores de haskell son inmutables, tenemos que crear un nuevo valor jugador con los datos actualizados
               --let jugador2 = Jugador { nombre=nombre2, estaJugando=True, esGanador=False, fichas=3}
               --print (nombre jugador1 ++ " " ++ mostrarEstaJugando (estaJugando jugador1))
               --print (nombre jugador2 ++ " " ++ mostrarEstaJugando (estaJugando jugador2))
               turnoJ1 jugador1 jugador2 3 3

mostrarEstaJugando x = if x then "1" else "0"

{- 
- Mientras el Jugador que tenga el turno tenga fichas:
- Hacer la jugada
- validar que no haya fichas antes
- Restar 1 ficha
- Actualizar el tablero segun la ficha del jugador
- Pasar turno
 -}

turnoJ1 jugador1 jugador2 fichasJ1 fichasJ2 = do if (fichasJ1 > 0) then do
                                                  putStrLn ("Turno de " ++ nombre jugador1 ++ ". Fichas restantes: " ++ show (fichasJ1))
                                                  putChar '\n'
                                                  tupla <- ingresarCord
                                                  putStrLn ("la tupla ingresada es: " ++ show (tupla) )
                                                  turnoJ2 jugador2 jugador1 fichasJ2 (fichasJ1 - 1)
                                                 else turnoJ2 jugador2 jugador1 fichasJ2 fichasJ1
                                                 
turnoJ2 jugador2 jugador1 fichasJ2 fichasJ1 = do if (fichasJ2 > 0) then do
                                                   putStrLn ("Turno de " ++ nombre jugador2 ++ ". Fichas restantes: " ++ show (fichasJ2))
                                                   putChar '\n'
                                                   turnoJ1 jugador1 jugador2 fichasJ1 (fichasJ2 - 1)
                                                 else do putStrLn ("Termino el juego")

ingresarCord = do putStrLn ("Ingrese nro de col")
                  col <- getLine
                  putStrLn ("Ingrese nro de fila")
                  fila <- getLine
                  return (((read col)::Int),((read fila)::Int))

-- turnoJ1 jugador1 = do putStrLn ("Turno de " ++ nombre jugador1 ++ ". Fichas restantes " ++ fichas jugador1)
--                       if (fichas jugador1 > 0) then              
              
-- turnoJ2 jugador2 = do putStrLn ("Turno de " ++ nombre jugador2 ++ ". Fichas restantes " ++ fichas jugador2)
--                       if (fichas jugador2 > 0) then   
----- Imprimir tablero en pantalla ------

impTab [] = do putStrLn ""
impTab (xs:xss) = do putChar '|'
                     impLin xs
                     putChar '\n'
                     impTab xss

impLin [x] = do putStr . show $ x
                putChar '|'
                
impLin (x:xs) = do putStr . show $ x
                   putChar '|'
                   impLin xs

--generarIndice xs

----- generar tablero vacio --------

agregarFilas 0 cols = []
agregarFilas filas cols = if filas > 0 then agregarCols cols : agregarFilas (filas-1) cols
                                  else agregarFilas filas cols

agregarCols 0 = []
agregarCols cols = if cols > 0 then Empty : agregarCols (cols-1)
                              else agregarCols cols


------- Funciones de ayuda ------

--all recibe una condicion y una lista y retorara true si todos los elementos de la lista cumplen la condicion
todosIguales (x:xs) = all (==x) xs

diagUR xs = [(xs!!n)!!n | n <- [0..(length xs) -1]]

main = threeline
 