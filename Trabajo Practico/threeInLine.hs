import System.IO

data Ficha = Cruz | Circ | Empty deriving (Eq)


instance Show Ficha where
  show Cruz = "X"
  show Circ = "O"
  show Empty = " "

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

threeline = do putStrLn "Ingrese nombre del jugador 1"
               nombre1 <- getLine
               putStrLn "Ingrese nombre del jugador 2"
               nombre2 <- getLine
               let jugador1 = Jugador { nombre=nombre1, estaJugando=True, esGanador=False}
               let jugador2 = Jugador { nombre=nombre2, estaJugando=False, esGanador=False}
               print (nombre jugador1 ++ " " ++ mostrarEstaJugando (estaJugando jugador1))
               print (nombre jugador2 ++ " " ++ mostrarEstaJugando (estaJugando jugador2))
               let jugador1 = Jugador { nombre=nombre1, estaJugando= False, esGanador=False}
               let jugador2 = Jugador { nombre=nombre2, estaJugando=True, esGanador=False}
               print (nombre jugador1 ++ " " ++ mostrarEstaJugando (estaJugando jugador1))
               print (nombre jugador2 ++ " " ++ mostrarEstaJugando (estaJugando jugador2))

mostrarEstaJugando x = if x then "1" else "0"

----- Imprimir tablero en pantalla ------

impTab [] = do putStrLn ""
impTab (xs:xss) = do impLin xs
                     putChar '\n'
                     putStrLn (generar 10)
                     putChar '\n'
                     impTab xss


impLin [x] = do putStr . show $ x
impLin (x:xs) = do putStr . show $ x
                   putChar '|'
                   impLin xs

generar 0 = []
generar n = '_' : generar (n-1)

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
 