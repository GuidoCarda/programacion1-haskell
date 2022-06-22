import Data.Char
import System.IO

{-

  Que es IO? 

  Una accion IO cuando se utiliza, cargara con unca accion
  con algun efecto secundario, como puede ser la entrada
  de datos o mostrar por pantalla y contendra algun tipo
  de valor dentro de el

  Mostrar algo por pantalla no tiene ningun tipo de resultado
  por lo tanto se usa el valor ficticio ()

  La tupla vacía tiene el valor de () y también tiene el tipo (). Algo como data Nada = Nada.

  Cuando se ejecuta una accion IO?

  Una accion IO se ejecuta cuando le damos el nombvre main
  y ejecutamos nuestro programa/

  Para poder utilizar varias acciones IO utilizamos
  la sintaxis do para juntar varias acciones en una
-}


-- main = do
--   putStrLn "Mostrar String en pantalla"
--   putStrLn "Ingresa un nombre"
--   firstName <- getLine
--   putStrLn ("El nombre es " ++ firstName)
--   let upperFirstName = map toUpper firstName
--   putStrLn ("El nombre ingresado en mayusculas es " ++ upperFirstName)


{-
  :t of putStrLn
  putStrLn :: String -> IO ()
 
  Esta toma una cadena y devuerlve una accion
  IO que retorna un tipo (), es decir la tupla vacia
  tambien conocida como unidad.

  getLine

  Accion IO que contiene un resultado del tipo String.

  nombre <- getLine

  esta linea lo que hace es realizar la accion
  getLine y luego ligar el resultado al valor name.

  Como getLine tiene el tipo IO String

  name sera del tipo String
-}


 
 
fortuna = do 
  putStrLn "Ingresa tu nombre y te dire tu fortuna"
  name <- getLine
  putStrLn ("Lee esto cuidadosamente, porque este es tu fururo: " ++ tellFortune name)

tellFortune name | name == "guido" = "No vas a usar haskell en tu puta vida"
                 | name == "joaquin" = "Dejar de jugar a los jueguitos deberas, si tu quieres programar"
                 | name == "mateo" = "Algun dia discord te funcionara, no pierdas la esperanzas"
                 | otherwise = "Ni idea xd"


leerLineas = do
  line <- getLine
  if null line
    then return ()
    else do
        putStrLn ("Hola que haces " ++ line)
        leerLineas

-- Como debemos tener una unica accion IO despues del
-- else utilizamos un bloque do para juntar todas las
-- acciones en una

data Jugador = Jugador { nombre::String, apellido::String, edad::String} deriving Show

crearJugador = do 
  putStrLn "Ingrese su nombre"
  name <- getLine
  putStrLn "Ingrese su apellido"
  lastName <- getLine
  putStrLn "Ingrese su edad"
  age <- getLine
  let jugador = crearRegistro name lastName age
  putStrLn (nombre jugador ++ " " ++ apellido jugador ++ " " ++ edad jugador)
  caracter <- getChar
  putChar caracter


crearRegistro name lastName age = Jugador { nombre=name, apellido=lastName, edad=age }



ahorcado = do
  putStrLn "Ingrese su nombre"
  name <- getLine
  putStrLn "Ingrese una palabra"
  word <- getLine
  putStrLn showWord word

showWord word = map word (/x -> "-") 