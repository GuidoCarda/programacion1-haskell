import System.IO

ahorcado = do putStrLn "Piense una Palabra"
              palabra <- sgetLine
              putStrLn "Intente Adivinar"
              adivinar 5 palabra ""

sgetLine :: IO [Char]
sgetLine = do hSetEcho stdin False
              palabra <- sgetLine'
              hSetEcho stdin True
              return palabra

sgetLine' = do x <- getChar
               if x == '\n' then do putChar x
                                    return []
                            else do putChar '-'
                                    xs <- sgetLine'
                                    return (x:xs)


adivinar contador palabra acierto = do putStr ("Intento "++show(contador)++" > ")
                                       xs <- getLine
                                       if (length xs) > 1 then arriesgar xs palabra
                                       else if (elem (head xs) palabra) then do putStrLn (diff palabra (acierto++xs))
                                                                                if ((diff palabra (acierto++xs)) == palabra) then putStr "Esa es la palabra. Ganaste!"
                                                                                else adivinar contador palabra (acierto++xs)
                                            else if contador == 0 then putStrLn "Maximo de intentos! Perdiste!"
                                                                  else adivinar (contador - 1) palabra acierto

diff xs ys = [if elem x ys then x else '-' | x <- xs]
arriesgar cadena palabra | cadena == palabra = putStr "Esa es la palabra. Ganaste!"
                         | otherwise = putStr "Perdiste"
main = ahorcado