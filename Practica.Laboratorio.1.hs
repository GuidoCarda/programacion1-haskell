module Practica0 where

import Data.Char
import Data.List

{-
1) Los siguientes códigos tienen errores, cargar el archivo en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)

-- Devolvia el mismo valor para ambos casos
regla b = case b of
            True -> "Quedate en Casa"
            False -> "Quedate en Casa"

-- b)
-- case' []          =  []
-- case' [x]         =  []
-- case' (x:y:xs)      =  y : case (x:xs)

-- Usa una palabra reservada como nombre de la funcion

-- c) Usa la palabra reservada map como nombre de la función
map' f []        =  []
map' f (x:xs)    =  f x : map f xs

{-

-- d)
-- No podemos aplicar la funcion cons con distintos tipos de datos. Los datos no matchean

--Funcion original  listNumeros = (1 : 2) : 'a' : []
listNumeros = 1 : (2 : (3 : []))

-- e)
-- El operador ++! no existe, por otra parte el operador ++ se utiliza para concatenar listas

funcionE [] ys     = ys
funcionE (x:xs) ys = x : xs ++ ys
-}

-- f)

--
--addToTail x xs = map +x tail xs
-- Agrega x a todos los elementos de la tail de la lista q pasemos como parametro
addToTail x xs = map (+x) (tail xs)

-- g)
-- Ordena una lista de forma ascendente y nos devuelve el valor minimo de esta
--listmin xs = head . sort xs

-- El punto se usa para la composicion de funciones
-- Si bien esta solucion es valida, el enunciado busca que utilize el . para componer funciones
{-listmin xs = head (sort xs)-}

listmin xs = (head . sort) xs

{-
Esto lo que hace es hacer primero la composicion de funciones y luego aplicar eso a la lista en cuestion
-}

-- h) (*)


-- El tipo de dato de la funcion smap crece en cada llamada, no se puede tener un tipo infinito

-- smap f [] = []
-- smap f [x] = [f x]
-- smap f (x:xs) = f x : smap (smap f) xs

-- El problema aca esta en que en el cuerpo de la funcion se refencia es decir se llama a la misma funcion 2 veces, esto proboca que la funcion termine necesitando de distintos tipos de datos resultando en un buble infinito


-- Macheo de patrones pattern matching

-- [] matchea con  una lista vacia
-- [x] matchea con una lista con un solo elemento
-- (x:xs) matchea con una lista que tiene al menos una cabeza y una cola

smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

{-

--2. Definir las siguientes funciones y determinar su tipo:

--a) five, que dado cualquier valor, devuelve 5
five n = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de
-- aplicar la función al valor dado
apply f y = f y


--c) identidad, la función identidad
identidad x = x

-- d) first, que toma un par ordenado, y devuelve su primera componente

first (x,y) = x

-- e) derive, que aproxima la derivada de una función dada en un punto dado
derive f x h = ( f (x+h) - f(x) ) / h


--f) sign, la función signo

sign :: Int -> Int
-- -v1 usando guardas
-- sign n | n < 0 = -1
--        | n == 0 = 0
--        | otherwise = 1

-- v2 usando un if tradicional
sign n = if n < 0 then -1 else
            if n == 0 then 0 else 1

-- g) vabs, la función valor absoluto (usando sign y sin usarla)

-- usando sign
vabs x = if sign x == -1 then -x else x
-- sin usar signvabs x = -x

-- h) pot, que toma un entero y un número, y devuelve el resultado de
-- elevar el segundo a la potencia dada por el primero

pot :: Int -> Int -> Int
pot x y = y ^ x


-- i) xor, el operador de disyunción exclusiva

--Disyuncion exclusiva Falso cuando ambos val de v sean iguales

xor :: Bool -> Bool -> Bool
xor v1 v2 = if v1 == v2 then False else True

xor v1 v2 | v1 == v2 = False
          | otherwise = True

-- j) max3, que toma tres números enteros y devuelve el máximo entre llos
-}

-- max3 n1 n2 n3 = max ( max n1 n2 ) n3
-- max3 x y z = maximum (x:(y:(z:[])))

max3 x y z | x > y && x > z = x
           | y > x && y > z = y
           | otherwise = z
{-
-- k) swap, que toma un par y devuelve el par con sus componentes invertidas

swap (x,y) = (y,x)

-}

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}
-- 365 dias Año normal
-- 366 dias año biciesto febrero tiene 29 dias

-- Se repite c/4 años
-- Exepcion del ultimo de cada siglo cuyo número de centenas no sea multiplo de 4

bisiesto :: Int -> Bool
bisiesto year = (year `mod` 4 == 0) && (year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- mod year 4 == 0  y mod year 100 /= 0
--                 o
-- mod year 4 == 0  y mod year 400 == 0

-- 1800 mod 4 = 0
-- 1800 mod 100 = 0

-- 1800 mod 4 = 0
-- 1800 mod 400 = 200

-- bisiesto :: Int -> Bool
-- bisiesto x | x `mod` 4 == 0 = True

--            | x `mod` 400 == 0 = True
--            | otherwise = False

--4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:

-- Las funciones que toman como argumento funciones se llaman funciones de alto orden


-- Funcion q recibe una funcion que recibe un entero y devuelve un entero


--a) (Int -> Int) -> Int
a4 f = (f 5) + 1


--  (Int -> Int) / funcion que recibe un entero y devuelve un entero


--  b) Int -> (Int -> Int)

-- Es una funcion que toma un Int y retorna (Int -> Int) que es una uncion que toma un Int y retorna un Int

-- b4 :: Int -> (Int -> Int)
-- Podemos rescribirla de la siguiente manera  b4 :: Int -> Int -> Int
-- Que se lee: b4 toma dos Int y retorna un Int

--  b4 :: Int -> (Int -> Int) == b4 :: Int -> Int -> Int

-- Esto es gracias a la currificacion, que permite la aplicacion parcial

-- b4 x y = 3 <- Esta mal porque puede recibir cualquier tipo de parametro

-- Cuando el enunciado pida Int contemplamos contemplamos floats tamb
--b4 x y = x + y


-- Otro ejemplo
-- b4' :: Int -> ( Int -> Int )

-- Si pasamos un solo parametro, ghci nos dira que falta un parametro
-- Ya que por la aplicacion parcial, si le pasamos un solo parametro
-- la funcion queda a la espera de otro parametro


b4' x = if even x then (x+)
                  else (x*)

--c) (Int -> Int) -> (Int -> Int)
-- Es lo mismo que decir ( Int -> Int ) -> Int -> Int

--c4 abs x = abs x
--c4 f x = if f x < 10 then (x+)
--                     else (x*)
--c4 f x = (+1) x
c4 f x = (f 2) + x
c4' f x = (f 2) `div` x


--d) Int -> Bool
d4 x = even x

--e) Bool -> (Bool -> Bool)

-- Por currificacion es lo mismo que∷

-- Bool -> Bool -> bool

-- v1
-- e4 x y = if x == True && y == True then True
--                                    else False


-- v2
e4 x y | x == True && y == True = True
       | otherwise = False


-- f) (Int,Char) -> Bool

--f4 (x,y) = if x < 10 && y < 'k' then True
--                                else False

f4 (x,y) = (x < 10) && (y < 'k')


--g) (Int,Int) -> Int
g4 (x,y) = if x+1 == y+1 then 1
                         else 0

--h) Int -> (Int,Int)
h4 n = (n `div` 2,n `div` 3)

--i) a -> Bool

i4 x = x == x

-- Puede recibir cualquier tipo y la guarda devuelve un booleano como resultado


--j) a -> a

d1 x = case x of
            _ -> x

primera x = (x,x)
segunda (x,y) = y

ff x = segunda (primera x)

-- 5) Definir las siguientes funciones usando listas por comprensión:

-- a) 'divisors', que dado un entero positivo 'x' devuelve la
-- lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

divisors x = [ n | n <- [1..x], x `mod` n == 0]

-- divisoresPropios cuando no se incluye el listNumeros

{-\  TAREA  \-}

-- Numeros abundantes: Un numero es abundante cuando este es menor a la suma de sus divisores divisoresPropios
--Crear la lista de toos los numeros abundantes usando listas por comprension
divisoresPropios x = [ n | n <- [1..x-1], x `mod` n == 0]
abundantes = [ n | n <- [0..], n < sum (divisoresPropios n) ]

{-\ FIN TAREA \-}

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta
-- de la lista los elementos distintos a 'x'

matches x [] = []
matches x xs = [ n | n <- xs, x == n ]

-- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
-- '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
-- donde 0 <= a, b, c, d <= 'n'

cuadrupla n = [ (a,b,c,d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], (a^2 + b^2) == (c^2 + d^2)]

-- (d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
-- 'xs' sin elementos repetidos
-- unique :: [Int] -> [Int]

unique [] = []
unique (x:xs) = if elem x xs then unique xs else x:unique xs

esta x [] = False
esta x (y:ys) = if x == y then True else esta x ys

unique2 [] = []
unique2 (x:xs) = if esta x xs then unique xs else x: unique xs 
{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}


scalarProduct xs ys = sum [ x*y | (x,y) <- zip xs ys ]

{-


7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

--a) 'suma', que suma todos los elementos de una lista de números
-}

suma [] = 0
suma (x:xs) = x + suma xs

-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario
alguno :: [Bool] -> Bool
alguno []     = False

-- Version usando if
-- alguno (x:xs) = if x == True then True else alguno xs

-- Version usando guardas
alguno (x:xs) | x == True = True
              | otherwise = alguno xs


-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario
-- [] -> False
-- [True,True,True,True] -> True
-- [True,True,False,True] -> False

todos :: [Bool] -> Bool
todos []     = False
todos [x]    = if x == True then True else False
todos (x:xs) = if x == True then todos xs else False



-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado


restos :: Int -> [Int] -> [Int]
restos n [] = []
restos n (x:xs) = x `mod` n : restos n xs


-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados

cuadrados []     = []
cuadrados (x:xs) = x ^ 2 : cuadrados xs


-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes


longitudes [] = []
longitudes (x:xs) = length x : (longitudes xs)



-- h) 'orden', que dada una lista de pares de números, devuelve
-- la lista de aquellos pares en los que la primera componente es
-- menor que el triple de la segunda

orden [] = []
orden ((x,y):xs) = if x < y*3 then (x,y): orden xs else orden xs



-- i) 'pares', que dada una lista de enteros, devuelve la lista
--de los elementos pares

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) = if even x then x: pares xs else pares xs


-- j) 'letras', que dada una lista de caracteres, devuelve la
-- lista de aquellos que son letras (minúsculas o mayúsculas)

-- letras :: [Char] -> [Char]
-- letras [] = []
-- letras (x:xs) = if isLetter x 

-- No se porque la funcion isLetter no funciona.


-- k) 'masDe', que dada una lista de listas 'xss' y un
-- número 'n', devuelve la lista de aquellas listas de 'xss'
-- con longitud mayor que 'n' 

masDe [] n = []
masDe (xs:xss) n = if length xs > n then xs: masDe xss n else masDe xss n  

{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}




-- 7) Sin usar funciones definidas en el
-- preludio, defina recursivamente las siguientes funciones y
-- determine su tipo más general:

--a) 'suma', que suma todos los elementos de una lista de números

suma' xs = foldr (+) 0 xs 


-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario

alguno' xs = foldr  (||) False xs

-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario

todos' xs = foldr (&&) True xs

-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales

-- codes' xs = map (+1) xs 

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado

restos' xs n = map (\x -> x `mod` n) xs

-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados

-- cuadrados' xs = map (^2) xs
-- Es lo mismo que: 
cuadrados' xs = map (\x -> x^2) xs

-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes

longitudes' xss = map (length) xss

-- h) 'orden', que dada una lista de pares de números, devuelve
-- la lista de aquellos pares en los que la primera componente es
-- menor que el triple de la segunda

orden' xs = filter (\(x,y) -> x < 3*y ) xs

-- i) 'pares', que dada una lista de enteros, devuelve la lista
--de los elementos pares

-- pares' xs = filter (even) xs
pares' xs = filter (\x -> x `mod` 2 == 0) xs

-- j) 'letras', que dada una lista de caracteres, devuelve la
-- lista de aquellos que son letras (minúsculas o mayúsculas)

letras' xs = filter (\x -> elem x (['a'..'z']++['A'..'Z']) ) xs

-- la funcion definida en el preludio elem devuelve true si el elemento x esta en la lista xs que le pasemos como parametro


-- k) 'masDe', que dada una lista de listas 'xss' y un
-- número 'n', devuelve la lista de aquellas listas de 'xss'
-- con longitud mayor que 'n' 

masDe' xss n = filter (\xs -> length xs > n) xss

