comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
comparing p x y = compare (p x) (p y)

forever     :: (Applicative f) => f a -> f b
forever a   = let a' = a *> a' in a'



nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)

nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)

sort :: (Ord a) => [a] -> [a]
sort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as


groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                               where (ys,zs) = span (eq x) xs


type Scale = [Int]

-- Es la longitud de la escala cromática: el cardinal del dominio.
long :: Int
long = 12


main :: IO String
main
  = forever
  $ do
      scale <- getLine
      putStrLn
        $ show
        $ function
        $ readScale scale

function :: Scale -> Scale
function scale =
      showResult
    $ nub
    $ induce
      scale



readScale :: String -> Scale
readScale scale
  = nub
  $ sort
  $ (\x -> if length x > 12 then [] else x)
  $ map
    (flip mod long . fst . head)
  $ filter
    (not . null)
  $ map reads
  $ words
    scale


-- Mostrar el resultado del cálculo.
showResult :: [Scale] -> Scale
-- `best` requiere no vacío.
-- Si ponemos la restricción en `best`, entonces falla head aquí.
showResult []
  = []

showResult scales
  = head
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
      = head  -- El primer grupo tiene la mínima distancia.
      $ groupBy
        (\x y -> snd x == snd y)
      $ sortBy
        (comparing snd)  -- La menor distancia.
      $ map
        (\s -> (s, norm s))
        scales


norm :: Scale -> Int
norm s
  = sum  -- La suma de las distancias
  $ map  -- entre la función y la escala cromática.
    (\(x, i) -> abs (x-i))
  $ zip s [0..]
  -- = sqrt
  -- $ fromIntegral
  -- $ sum
  -- $ map
  --   (\(x, i) -> (x-i)*(x-i))
  -- $ zip s [0..]


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

-- TODO importarlo de sitio existente
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
-- c es la frecuencia menor.
-- long = qc + (e - q)(c + 1)
func :: Int -> Scale -> [Scale]
func nfixed scale
  = let e = length scale
        c = div long e
    in do
      fixed <-
        combinations nfixed scale
      minFreq <-
        dissonances scale
      start <-  --[[h], [h,h], [h,h,h]]
        let
        h = head fixed
        hFreq =
          if h `elem` minFreq
            then c
            else c + 1
        in
        take hFreq $ iterate (++ [h]) [h]
      -- Quito los que tienen el 2o fixed dentro del ámbito del 1o.
      guard
        (length fixed == 1
        || (fixed !! 1) - (head fixed) >= length start)
      result <-
        let
        h = head fixed
        ( (first, freqFirst) : oldFreqs )
          = map
            (\ha ->
              if ha `elem` minFreq
                then (ha, c)
                else (ha, c + 1)
             )
          $ take e
          $ dropWhile (< h)
          $ cycle
            scale
        freqs  -- Ponemos el h al final con su frecuencia ya restada.
          = oldFreqs
          ++ if freqFirst /= length start
              then [ (first, freqFirst - length start) ]
              else []
        newfixed
          = take (nfixed - 1)
          $ dropWhile (<= h)
          $ cycle
            fixed
        in return $ oneCase newfixed freqs start
      guard
        (length result == long) -- Quito los que no tengan long (los []).
      return $ normalize result


-- Pongo el índice 0 otra vez al principio
-- y lo pongo en orden creciente.
normalize :: Scale -> Scale
normalize []
  = []
normalize result@(h:_)
    =  map (\i -> if i >  h then i - long else i) before
    ++ hh
    ++ map (\i -> if i <= h then i + long else i) after
  where
    (hhafter, before)
      = splitAt (long - h) result
    (hh, after)
      = span (== h) hhafter


-- Dados los puntos fijos, las frecuencias y el resultado
oneCase :: Scale -> [(Int, Int)] -> Scale -> Scale
oneCase _ _ []
  = []  -- Si no hay start ha habido un problema. No puede ocurrir.
oneCase _ [] x
  = x   -- Si no quedan frecuencias, hemos acabado bien.
oneCase [] ((next, nextFreq):freqs) start
  =     -- Si no quedan fixed points, simplemente rellenar.
  oneCase
    []
    freqs $
    start ++ replicate nextFreq next

-- Si quedan fixed points:
oneCase ffixed@(f:fixed) ((next, nextFreq):freqs) start@(s:_)
    -- Si f es igual al siguiente índice, next tiene que ser ese índice porque f es fijo.
    | f==currentIndex && f==next
    = oneCase
        fixed  -- Ya hemos usado f; lo desechamos.
        ((eraseFreq next nextFreq) ++ freqs) $
        start ++ [next]
    --Si no necesito fijar, entonces next no puede pasarse del siguiente fixed point f.
    | f/=currentIndex && f>=next
    = oneCase
        ffixed
        ((eraseFreq next nextFreq) ++ freqs) $
        start ++ [next]
    | otherwise = []
  where
    currentIndex
      = s + length start
    eraseFreq element freq
      = if freq == 1
          then []
          else [(element, freq-1)]
