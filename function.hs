import Control.Monad (forever,guard)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List
import Debug.Trace (trace)

type Escala = [Int]

long :: Int
long = 12

-- El gran if es para doble hueco
showE :: Escala -> String
showE [] = ""
showE e = -- tail para el primer " "
  tail $ foldl (\acc i ->
    acc ++ " " ++ show i ++ if i >= 0 && i < 10 then " " else "") "" e

showEscalas :: [Escala] -> String
showEscalas [] = ""
showEscalas e = (tail $ foldl (\acc i -> acc ++ "\n" ++ showE i) "" e)

showEs :: [Escala] -> String
showEs [] = ""
showEs e =
  let (sol,i) = eleccion e
  in (showEscalas e)
  ++ ("\n\nMayor ajuste (" ++ show i ++ "):\n")
    ++ (showEscalas sol)
    ++ if length sol > 1 then
      ("\n\nLa más grave de ellas es:\n")
      ++ (showE $ head $ sort sol)
    else ""

eleccion :: [Escala] -> ([Escala],Int)
eleccion escs = --head $ sort escs
  let solucion =
        head $ groupBy (\x y -> snd x == snd y) $ sortBy (comparing snd) $ map (\e -> (e,
          sum $ map (\(x,i) -> abs (x-i)) $ zip e [0..]
        )) escs
  in (map fst solucion, head $ map snd solucion)

main :: IO String
main = forever $ do
    esc <- getLine
    putStrLn "Calculando...\n"
    putStrLn $ showE [0..(long-1)]
    putStrLn $ replicate (long*3-1) '―'
    putStrLn $ showEs $ nub $ inducir $ toEscala esc
    putStrLn "\nTerminado."

toEscala :: String -> Escala
toEscala esc = map (flip mod long . read) $ words esc

-- Llama a la función recursiva
inducir :: Escala -> [Escala]
inducir esc = fxp (length esc) esc []

-- Obliga a que haya alguna fixed
-- ¿Existe alguna solución que no tenga 1 fixed pero sí 0?
-- He puesto demasiados head fixed, supongamos que no
fxp :: Int -> Escala -> [Escala] -> [Escala]
fxp _ _ (x:xs) = x:xs --si encontró solución, devuélvelo
fxp 0 _ _ = [] --si 0 fixed points, nada
fxp n esc _ = fxp (n-1) esc (func n esc) --si quedan fixed points, llama a func y disminuye n

-- #26 de H-99
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

dissonances :: Escala -> [Escala]
dissonances esc =
  let e = length esc
      q = e - mod long e
  in combinations q esc

--qc + (e-q)(c+1) = long
func :: Int -> Escala -> [Escala]
func nfixed esc =
  let e = length esc
      c = div long e
  in
  do fixed <- combinations nfixed esc
     minfrec <- dissonances esc
     start <- -- Posibles comienzos
        let h = head fixed
            hfrec =
              if h `elem` minfrec
                then c
                else c + 1
        in take hfrec $ iterate (++ [h]) [h] --[[h], [h,h], [h,h,h]]
     guard (length fixed == 1 || (fixed !! 1) - (head fixed) >= length start)
     -- Quito los que tienen el segundo fixed dentro del ámbito del primero
     caso <-
        let h = head fixed
            ((first,frecfirst):frecsx) =
              map (\ha -> if ha `elem` minfrec then (ha,c) else (ha,c+1)) $
              take e $ dropWhile (< h) $ cycle esc
            frecs = frecsx ++ [(first, frecfirst - length start)]
            -- Ponemos el h al final con su frecuencia ya restada
            newfixed =
              take (nfixed-1) $ dropWhile (<= h) $ cycle fixed
        in return $ unCaso newfixed frecs start
     guard (length caso == long) -- Quito los que no tengan long (los [])
     return $ renormalizar caso -- Pongo el 0 otra vez al principio...

-- y lo pongo en orden creciente
renormalizar :: Escala -> Escala
renormalizar (h:caso) =
  let (posterior,antes) = splitAt (long-h) (h:caso)
      (haches,despues) = span (==h) posterior
  in (map (\i -> if i > h then i-long else i) antes)
  ++ haches
  ++ (map (\i -> if i <= h then i+long else i) despues)

-- Dados los puntos fijos, las frecuencias y el resultado
unCaso :: Escala -> [(Int,Int)] -> Escala -> Escala
unCaso _ _ [] = [] -- Si no hay start ha habido un problema. No puede ocurrir.
unCaso _ [] x = x  -- Si no quedan frecuencias hemos acabado
unCaso [] ((next, nextfrec):frecs) start = -- Si no quedan fixed points simplemente rellenar
  unCaso [] frecs $
    start ++ replicate nextfrec next
unCaso (f:fixed) ((next, nextfrec):frecs) (s:start) = -- Si quedan...
  if f == mod (1 + s + length start) long
  -- Si el f es el siguiente índice, el next tiene que ser ese índice, porque f es fijo.
    then if next == f
      then unCaso fixed ((if nextfrec == 1 then [] else [(next, nextfrec-1)]) ++ frecs) $
           (s:start) ++ [next]
      else []
    else if next <= f
    --Si no necesito fijar a continuación, entonces el next no puede haberse pasado del siguiente f
      then unCaso (f:fixed) ((if nextfrec == 1 then [] else [(next, nextfrec-1)]) ++ frecs) $
           (s:start) ++ [next]
      else []
