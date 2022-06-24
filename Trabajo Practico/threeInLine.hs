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



threeline = do putStrLn "Ingrese la cantidad de filas"
               filas <- getLine
               putStrLn "Ingrese la cantidad de columnas"
               cols <- getLine
               impTab (agregarFilas ((read filas)::Int) ((read cols)::Int))


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


agregarFilas 0 cols = []
agregarFilas filas cols = if filas > 0 then agregarCols cols : agregarFilas (filas-1) cols
                                  else agregarFilas filas cols

agregarCols 0 = []
agregarCols cols = if cols > 0 then Empty : agregarCols (cols-1)
                              else agregarCols cols


main = threeline
