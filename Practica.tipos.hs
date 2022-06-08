type Pos = (Int, Int)
type Elemento = Char
type Matriz = [[Elemento]]

tablero = [ ['1','2','3'],
            ['4','5','6'],
            ['7','8','9']
          ]

ver :: Pos -> Matriz -> Elemento
ver (f,c) t = (t!!f)!!c


{-

Clase -- 27 / 06

-- Declaraciones Data
Creamos un unevo tipo

data Bool = False | True


- Los constructores pueden estar acompañados de parametros

data Shape = Circle Float |  Rect Float Float

square :: Float -> Shape
square n = Rect n n


- Los constructores pueden ser vistos como funciones

Circle :: Float -> Shape
Rect :: Float -> Shape

 -}

data Shape = Circle Float | Rect Float Float
             deriving Show

-- cuando creamos un tipo de dato, siempre debemos poner deriving Show para que haskell lo pueda imprimir por pantalla

--cuadrado :: Float -> Shape
--cuadrado n = Rect n n


-- las declaraciones de data pueden tener tambien parámetros de tipos

-- las declaraciones data pueden tambien tener parametros de tipos

-- data Maybe a = Nothing | Just a

{-
-- La declaracion data puede ser recursiva
-}

data Nat = Zero | Suc Nat deriving Show

add n Zero = n
add n (Suc m) = Suc ( add n m )

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Suc ( int2Nat (n-1) )

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Suc n) = 1 + nat2Int n

--SUMA
-- DATA Nat=Zero o suc Nat=1
-- add :: Nat->Nat->Nat
-- add n zero = n
-- add n (Suc m ) = Suc (add n m)
--n+(m+1) = 1+(n+1)

-- Multiplicacion para Nat
-- mult :: Nat->Nat->Nat
-- mult n Zero = Zero
--mult n (suc m)

--PRUEBA
--mult Zero m = Zero
--mult (Suc n) m = Suc ( add n (mult n m-1))

mult :: Nat->Nat->Nat
mult n Zero = Zero
mult n (Suc m) = add n (mult n m)


pot :: Nat->Nat->Nat
pot n Zero = (int2Nat 1)  -- Devuelvo 1 ya que todo numero elevado a 0 es 1. Uso la funcion int2Nat para pasar el uno de int a natural
pot n (Suc m) = mult n (pot n m) -- La potencia es igual a n * n, m veces


-- n * n * n * n 
--    m veces

-- n 2
-- n*n







-- Exponenciacion para Nat



{----- Practica 2 -----}

--Ejercicio 1

-- Forma 1 usando Type
type Color1 = (Int,Int,Int)

mezclar1 :: Color1 -> Color1 -> Color1
mezclar1 (r1,g1,b1) (r2,g2,b2) = ( (div (r1+r2) 2), (div (g1+g2) 2), (div (b1+b2) 2))

-- Forma 2 usando Data
data Color2 = RGB Int Int Int deriving Show

mezclar2 :: Color2 -> Color2 -> Color2
mezclar2 (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (div (r1+r2) 2) (div (g1+g2) 2) (div (b1+b2) 2)

--Forma 3 usando registros
data Color3 = Color { red :: Int
                    , green :: Int
                    , blue :: Int 
                    } deriving Show

mezclar3 :: Color3 -> Color3 -> Color3
mezclar3 (Color { red = r1, green = g1, blue = b1}) (Color { red = r2, green = g2, blue = b2}) = 
  Color { red = ( div (r1+r2) 2), green = (div (g1+g2) 2), blue = (div (b1+b2) 2) }  

-- mezclar3 (Color {red=150,blue=0,green=0}) (Color {red=230,blue=0,green=0})

-- Forma 4 usando types

type IntColor = Int
type Color4 = ( IntColor, IntColor , IntColor)

mezclar4 :: Color4 -> Color4 -> Color4
mezclar4 (r1,g1,b1) (r2,g2,b2) = ((div (r1+r2) 2), (div (g1+g2) 2), (div (b1+b2) 2))