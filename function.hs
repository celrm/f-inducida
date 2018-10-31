import Control.Monad (forever, guard)
import Data.List
import Data.Ord (comparing)
--import Data.Function (on)
--import Debug.Trace (trace)


type Scale = [Int]


-- Es la longitud de la escala cromática: el cardinal del dominio.
long :: Int
long = 12


-- Mostrar una escala.
showScale :: Scale -> String
showScale []
  = ""

showScale scale
  = tail  -- El tail quita el primer espacio.
          -- Por eso se necesita separar entre escala vacía y no vacía.
  $ foldl
    (\acc i ->
      acc
      ++ " "
      ++ show i
      -- Deja doble hueco en los números de una cifra.
      ++ if i >= 0 && i < 10
          then " "
          else ""
    ) "" scale


-- Mostrar varias escalas.
showScales :: [Scale] -> String
showScales []
  = ""

showScales scales
  = tail
  $ foldl
    (\acc sc ->
      acc
      ++ "\n"
      ++ showScale sc
    ) "" scales


-- Mostrar el resultado del cálculo.
showResult :: [Scale] -> IO ()
showResult []
  = return ()

showResult scales
  = do
      putStrLn
        $ showScale [ 0 .. (long - 1) ]
      putStrLn
        $ replicate (long * 3 - 1) '―'
      putStrLn
        $ showScales scales
      putStrLn
        ( "\nMayor ajuste ("
        ++ show bestPunct
        ++ "):"
        )
      putStrLn
        $ showScales bestFits
      putStrLn
        "\nLa más grave de ellas es:"
      putStrLn
        $ showScale
        $ head
        $ sort
          bestFits
    where
      (bestFits, bestPunct) =
        best scales


best :: [Scale] -> ([Scale],Int)
best scales
  = ( map fst solution
    , head $ map snd solution
    )
  where
    solution
      = head  -- El primer grupo con la mínima distancia.
      $ groupBy
        (\x y -> snd x == snd y)
      $ sortBy
        (comparing snd)  -- La menor distancia.
      $ map
        (\e ->
          (e, sum  -- La suma de las distancias
            $ map  -- entre la función y la escala cromática.
              (\(x, i) -> abs (x-i))
            $ zip e [0..]
          )
        )
        scales


main :: IO String
main
  = forever
  $ do
      scale <- getLine
      putStrLn
        "Calculando...\n"
      showResult
        $ nub
        $ induce
        $ toScale
          scale
      putStrLn
        "\nTerminado."


toScale :: String -> Scale
toScale scale
  = map
    (flip mod long . fst . head)
  $ filter (not . null)
  $ map reads
  $ words
    scale


-- Llama a la función recursiva de fixed points.
induce :: Scale -> [Scale]
induce scale =
  fxp (length scale) scale []


-- Obliga a que haya alguna fixed.
-- ¿Existe alguna solución que no tenga 1 fixed pero sí 0?
-- He puesto demasiados head fixed, supongamos que no.
fxp :: Int -> Scale -> [Scale] -> [Scale]
-- Si encontró solución, devuélvelo.
fxp _ _ xxs@(_:_)
  = xxs
-- Si 0 fixed points, no hay solución.
fxp 0 _ _
  = []
-- Si quedan fixed points, llama a func y disminuye nfixed.
fxp nfixed scale _
  = fxp (nfixed - 1) scale (func nfixed scale)


-- #26 de H-99
combinations :: Int -> [a] -> [[a]]
combinations 0 _
  = [[]]
combinations _ []
  = []
combinations n (x:xs)
  = map
    (x:)
    (combinations (n-1) xs)
    ++ combinations n xs


dissonances :: Scale -> [Scale]
dissonances scale
  = combinations q scale
  where
    e = length scale
    q = e - mod long e


-- q es el tamaño del subconjunto de frecuencias menores.
-- c la frecuencia menor.
-- long = qc + (e - q)(c + 1)
func :: Int -> Scale -> [Scale]
func nfixed scale
  = let e = length scale
        c = div long e
    in do
      fixed <-
        combinations nfixed scale
      minfrec <-
        dissonances scale
      start <-  --[[h], [h,h], [h,h,h]]
        let
        h = head fixed
        hfrec =
          if h `elem` minfrec
            then c
            else c + 1
        in
        take hfrec $ iterate (++ [h]) [h]
      -- Quito los que tienen el 2o fixed dentro del ámbito del 1o.
      guard (length fixed == 1 || (fixed !! 1) - (head fixed) >= length start)
      caso <-
        let
        h = head fixed
        ( (first, frecfirst) : frecsx )
          = map
            (\ha ->
              if ha `elem` minfrec
                then (ha, c)
                else (ha, c + 1)
             )
          $ take e
          $ dropWhile (< h)
          $ cycle
            scale
        frecs  -- Ponemos el h al final con su frecuencia ya restada.
          = frecsx
          ++ [ (first, frecfirst - length start) ]
        newfixed
          = take (nfixed - 1)
          $ dropWhile (<= h)
          $ cycle
            fixed
        in return $ unCaso newfixed frecs start
      guard (length caso == long) -- Quito los que no tengan long (los []).
      return $ renormalizar caso

-- Pongo el 0 otra vez al principio
-- y lo pongo en orden creciente.
renormalizar :: Scale -> Scale
renormalizar []
  = []
renormalizar hcaso@(h:_)
    =  map (\i -> if i >  h then i - long else i) antes
    ++ haches
    ++ map (\i -> if i <= h then i + long else i) despues
  where
    (posterior, antes)
      = splitAt (long - h) hcaso
    (haches, despues)
      = span (== h) posterior


-- Dados los puntos fijos, las frecuencias y el resultado
unCaso :: Scale -> [(Int, Int)] -> Scale -> Scale
unCaso _ _ []
  = []  -- Si no hay start ha habido un problema. No puede ocurrir.
unCaso _ [] x
  = x   -- Si no quedan frecuencias, hemos acabado bien.
unCaso [] ((next, nextfrec):frecs) start
  =     -- Si no quedan fixed points, simplemente rellenar.
  unCaso [] frecs $
    start ++ replicate nextfrec next

unCaso (f:fixed) ((next, nextfrec):frecs) (s:start)
  = -- Si quedan...
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
