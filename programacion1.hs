fact 0 = 1
fact n = n * fact ( n - 1 )

main = do
  putStrLn "Hello world"


ones 0 = []
ones n = 1 : ones ( n - 1 )

-- : para agregar un elemento a el array 

-- :q para salir del archio en ghci
-- :r recargar el archivo en ghci



{-
 Matematica    Haskell
  f(x)           f x
  f(x,y)         f x y
  f(g(x))        f (g x)
  f(x,g(y))      f x (g y)
  f(x)*g(y)      f x * g y
  (f.g)(x)       (f o g) x
-}

-- La aplicaciòn de funciones tiene mayor precedencia
-- Las funciones y sus argumentos deben comenzar en minusculas

-- Por convencion los nobres de las listas terminan en s, para decir que tienen multiples valores


suma x y = x + y

sumar (x,y) = x + y


-- Si doy los parametros en una tupla, no puedo dejar un campo vacio al llamar la función
-- Por ejemplo sumar (1) va a dar error ya que estoy pasando un unico valor en la tupla


{-
  En una serie de definiciones, cada definicion debe empezar en la misma columna

  a = b + c
      where
        b = 1
        c = 1
  d = a * 2
-}

-- Tipos de datos
{-
  Bool
  True :: Bool
  False :: Bool

  Bool: Booleanos
  Int: Enteros de precisión fija
  Integer: Enteros de precisión arbitraria
  Char: Caracteres
  Float: numeros de punto flotante de presición simple
-}

-- div funcion que hace la division entera entre 2 params

-- Listas

{-
  Una lista es una secuencia de valores del mismo tipo
  No hay restricción respecto de la longitus de la lista
-}

-- Tuplas

{-
 Una tupla es una secuencia finita de valores (posiblemente) distintos

  (True, True) :: (Bool,Bool)
  (True, 2)    :: (Bool, Int)

A diferencia de las listas, las tuplas tienen explicitado en su tipo la cantidad de elementos que almacenan
-}

-- El tipo función

{-
  Una funcion mapea valores de un tipo a otro

Signatura de tipo de una función: Explicitar el tipo que tienen los parametros y la salida de una función

ejemplo

ones :: Int ->  [Int]
ones 0 = []
ones n = 1 : ones ( n - 1 )
-}


-- Currificación
{-
  Es posible definir una funcion con multiples argumentos retornando una funcion como resultado


  Currificación: Cuando los argumentos de una funcion los damos separados por espacios
  Esto permite la aplicación parcial.

Al pasarle un argumento a la funcion, la funcion nos va a devolver otra funcion que va a esperar los argumentos para ser ejecutado


ej 

add :: Int -> ( Int -> Int )
sumar x y = x + y

-}




-- Clase 08/04

{-
  FUnciones polimorficas:
  Una funcion es polimorfica si su tipo contiene una o mas variables de tipo

 length :: [a] -> Int

[a] es una variable 

las variables de tipo pueden ser instanciadas a otros tipos
ej

length [False, True] :: a == Bool
length ['a', 'b'] :: a == Char


Las variables de tipo siempre en minusculas
-}



factorial 0 = 1
factorial n = n * factorial (n-1)

sumatoria 1 = 1
sumatoria n = n + sumatoria (n-1)

hasta n = [0..n]

-- Clase 08/04

{-
  FUnciones polimorficas:
  Una funcion es polimorfica si su tipo contiene una o mas variables de tipo

 length :: [a] -> Int

[a] es una variable

las variables de tipo pueden ser instanciadas a otros tipos
ej

length [False, True] :: a == Bool
length ['a', 'b'] :: a == Char


Las variables de tipo siempre en minusculas
-}

--Asigntura de tipo de una funcion
--suma :: Int -> Int -> Int
--suma a b = a + b

--Expresiones condicionales
{-
 Laas funciones pueden ser definidas usando expresiones condicionales


 Porque ambas ramas de un condicional en una funcion deben devolver el mismo tipo de dato

 1 - Porque se rompe la asignatura de tipo
 2 - Porque se rompe la definicion de funcion matematica

-}

abs' :: Int -> Int
abs' n = if n < 0 then n else -n

-- Las expreciones condicionales pueden estar anidadas

sign :: Int -> Int
sign n = if n < 0 then -1 else
           if n == 0 then 0 else 1


-- Si vamos a pasar un numero negativo como parametro tiene que i r entre parentesis 

-- Una alternativa a los condicionales es el uso de ecuaciones con guardas

-- proposiciones = predicados

{-


abs | n > 0 = n
    | otherwise = -n

-- Se usan para hacer ciertas definiciones mas faciles

signum x | n < 0 = -1
         | n == 0 = 0
         | otherwise 1

-- La condicion otherwise se define en el prelucio como : otherwise = true

-- Si una funcion con condicional no tiene otherwise se la llama funcion parcial


-- Patter matching

 Muchas funciones se definen mas claramente usando pattern matching (Ajuste de patrones)

 not :: Bool -> Bool
 not False = True
 not True == False

 
 not mapea False a True y True a False

 Operadores logicos en haskell
 && AND
 || OR


 (^) :: Bool -> Bool -> Bool
 True ^ False = true


 puede ser escrita en forma de comparacion comparacion
 (^) :: Bool -> Bool -> Bool
  True ^ True = True
   _ ^ _ = False

   El simbolo _ es un patron comodin ue machea cualquier patron
   Las ecuaciones de patron matching se evaluan en orden (Top-Down)

   Por ej la siguiente def devolveria siempre False

   _ ^ _ = False
   True ^ True = True


   Las ecuaciones no pueden repetir variables

   --------------------
     
   Patrones de listas

   Toda lista no vacia se contruye usando el operador : llamado 'cons' que agrega un elemento al principio de la lista
   [1,2,3,4] resulta de 1:(2:(3:(4:[])))

   Por lo tanto puedo definir funciones usando el patron (x:xs)

   head -- Devuelve la cabeza de la listas
   head :: [a] -> a
   head (x:_) = x

   tail -- devuelve la cola de la lista
   tail :: [a] -> [a]
   tail (_:xs) = xs
-}


longitud [] = 0
longitud (x:xs) = 1 + longitud xs


sumal [] = 0
sumal (x:xs) = x + sumal xs



-- Expresiones lambda

{- Las funciones pueden construirse sin nombres usando expresiones lambda

\x -> x + x

Es la funcion que toma como entrada x y retorna el resultado x + x


las funciones lambda son utiles para evitar darle nombres a funciones q van a ser utilizadas una sola vez

-- map aplica una funcion a cada elemento de la listas

ej map (+1) [1,2,3,4]
  >  [2,3,4,5]

  odd numeros impares

-}


-- Secciones

{-
  Infijo 
  Prefijo
-}


length' [] = 0
lenght' (_:xs) = 1 + lenght' xs


-- Listas por comprension

lista = [x^2|x<-[1..10]]

-- No es lo mismo:
tupla = [(x,y) | x<-[1,2,3], y<-[4,5]]
-- que: 
tupla' = [(x,y) | y<-[4,5] ,x<-[1,2,3] ]

-- Los multiples generadores actuan como bucles anidados. Los generadores posteriores cambian màs rapidamente


-- Un generador puede depender de variables introducidas por un generador anterior

 -- [(x,y) | y<-[4,5] ,x<-[1,2,3] ]


-- concat : aplana una lista

--  concat [[1,2,3],[],[4,5,6]]
-- devolvera [1,2,3,4,5,6]

factors n = [ x|x<-[1..n], n `mod` x == 0]

-- Toda funcion q tomeun valor y devuelve un boleano las llamaremos predicado
prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x<- [2..n], prime x]

--Funcion zip
--La funcion zip mapea dos listas a una lista con lospares de elementos correspondientes

 --zip :: [a] -> [b] -> [(a,b)] 

-- > zip ['a','b','c'] [1,2,3,4]
-- [('a,1')]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- Pares de adyacentes, tail devuelve todos los elementos menos el primero ej. Tail abc -> bc. 



ordenada xs = and [x<y|(x,y)<- pairs xs]

  