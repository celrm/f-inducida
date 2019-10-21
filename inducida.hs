import Control.Monad (forever, guard, replicateM)
import Data.List
import Data.Ord (comparing)
import Data.Function (on)

type Scale = [Int]

-- Longitud de la escala cromática.
long :: Int
long = 12

-- Dominio.
-- Debe pertenecer a la cromática.
superscale :: Scale
superscale =
  [0..long-1]
  --[0,2,5]

-- Longitud del dominio
suplong :: Int
suplong = length superscale

-- Decide si la función usa cualquier nota (induciendo la mejor raíz)
-- o si usa solo las notas de la subescala.
induceRoot :: [Int]
induceRoot =
  [-2*long,-long,0,long] -- Mismo conjunto.
  --[-2*long+1..long-1] -- Induce raíz.

---------------------------------------------------------------------

main :: IO String
main
  = forever
  $ do
      scale <- getLine
      putStrLn
        "Calculando...\n"
      showResult
        $ nub -- Quita repetidos.
        $ induce
        $ readScale
          scale
      putStrLn
        "\nTerminado."

---------------------------------------------------------------------

distance :: Scale -> Scale -> Int
distance sc1 sc2
  = sum  -- La suma de las distancias
  $ fmap  -- entre dos funciones.
    (\(x, y) -> abs (x-y))
  $ zip sc1 sc2
-- Normalmente usado con superscale

-- Combinaciones con repetición de longitud n
combinations :: Int -> [a] -> [[a]]
combinations 0 _
  = [[]]
combinations _ []
  = []
combinations n (x:xs)
  = fmap
    (x:)
    (combinations (n-1) xs)
    ++ combinations n xs

-- Algoritmo de la división
-- con el resto expresado de todas las formas posibles
repetitions :: Int -> Scale -> [Scale]
repetitions n scale
  = let e = length scale in
  if n < e
    then combinations n scale -- Resto
  else
    fmap
    (\r -> scale ++ r) -- Sumo otra vez la escala
    (repetitions (n-e) scale)

-- Desplazamiento cíclico de la escala
rotations :: Scale -> [Scale]
rotations scale
  = foldr (\x acc->
    ( take suplong
    $ drop x
    $ scale ++ (fmap (\x -> x+long) scale)
    ):acc) [] [0..suplong-1]

-- Encuentra las funciones bien distribuidas
induce :: Scale -> [Scale]
induce [] = [[]]
induce scale
  = do
    repet <- -- Tomo una de las repeticiones
      repetitions suplong scale
    perm <- -- La ordeno y saco todos los posibles ciclos
      rotations $ sort repet
    una <- -- Asumo no vacío
      foldr (\x acc->
          (if last perm + x < 0 || head perm + x > long -1
            then acc -- No tiene ninguna nota en la escala cromatica propia
            else (fmap (\e -> e + x) perm):acc)) []
      induceRoot -- Cualquier subconjunto o solo las notas dadas.
    return $ una

---------------------------------------------------------------------

-- Leer una escala
readScale :: String -> Scale
readScale scale
  = fmap (fst . head)
  $ filter
    (not . null)
  $ fmap reads
  $ words
    scale

-- Mostrar una escala.
showScale :: Scale -> String
showScale scale
  = drop 1  -- Quita el primer espacio.
  $ foldl
    (\acc i ->
      acc
      ++ " "
      ++ (if i>=0 -- Espacio para el - de los negativos
          then " "
          else "")
      ++ (if (i > -10 && i < 10) -- Espacio para las 2 cifras
          then " "
          else "")
      ++ show i
    ) "" scale

-- Mostrar varias escalas.
showScales :: [Scale] -> String
showScales scales
  = drop 1
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
        $ showScale superscale
      putStrLn
        $ replicate (suplong * 4 - 1) '―'
      putStrLn
       $ showScales scales
      putStrLn
       $ "\nMayor ajuste ("
       ++ show bestPunct
       ++ "):"
      putStrLn
        $ showScales bestFits
      putStrLn
        "\nLa más grave es:"
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
  = ( solution
    , distance superscale $ head solution
    )
  where
    solution
      = head  -- El primer grupo tiene la mínima distancia.
      $ groupBy ((==) `on` distance superscale)
      $ sortBy (comparing . distance $ superscale)
      $ scales
