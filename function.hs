import Control.Monad --forever
import Data.Function --on
import Data.List
import Debug.Trace

type Escala = [Int]

long :: Int
long = 12

showE :: Escala -> String
showE [] = ""
showE e = tail $ foldl (\acc i -> acc ++ " " ++ show i ++ if i < 10 && i >= 0 then " " else "") "" e

showEs :: [Escala] -> String
showEs [] = ""
showEs e = tail $ foldl (\acc i -> acc ++ "\n" ++ showE i) "" e

main :: IO String
main = forever $ do
    esc <- getLine
    putStrLn "Calculando..."
    putStrLn $ showE [0..(long-1)]
    putStrLn $ replicate (long*3-1) '―'
    putStrLn $ showEs $ nub $ inducir $ toEscala esc
    putStrLn "Terminado"

toEscala :: String -> Escala
toEscala esc = map (flip mod long . read) $ words esc

inducir :: Escala -> [Escala]
inducir esc = fxp (length esc) esc []

--obliga a que haya alguna fixed. es esto obligatorio? -> he puesto demasiados head fixed
fxp :: Int -> Escala -> [Escala] -> [Escala]
fxp _ _ (r:rs) = r:rs --si está lleno, devuélvelo
fxp 0 _ _ = [] --si ronda de 0 fixed points, muere
fxp n c _ = fxp (n-1) c (func n c) --si quedan fixed points, llama a func y disminuye n

combinations :: Int -> [a] -> [[a]]
combinations k s =
  case (k,s) of
    (0,_) -> [[]]
    (_,[]) -> []
    (_,e:s') -> map (e :) (combinations (k - 1) s') ++ combinations k s'

dissonances :: Escala -> [Escala]
dissonances esc =
  let e = length esc
      r = e - mod long e
      total = combinations r esc
  in [ x | x <- total ]           --TODO not yet completed

func :: Int -> Escala -> [Escala]
func n esc =
  let e = length esc
      c = div long e
  in
  do fixed <- combinations n esc
     minfrec <- dissonances esc
     start <-
        let h = head fixed
            frech = c +
              if h `elem` minfrec
                then 0
                else 1
        in take frech $ iterate (++ [h]) [h] --[[h], [h,h], [h,h,h]]
     guard (length fixed == 1 || (fixed !! 1) - (head fixed) >= length start)
     caso <-
        let h = head fixed
            ((first,frecfirst):frecsx) =
              map (\ha -> if ha `elem` minfrec then (ha,c) else (ha,c+1)) $
              take e $ dropWhile (< h) $ cycle esc
            frecs = frecsx ++ [(first, frecfirst - length start)]
            newfixed =
              take (n-1) $ dropWhile (<= h) $ cycle fixed
        in [unCaso newfixed frecs start]
     guard (length caso == long)
     return $ renormalizar caso

renormalizar :: Escala -> Escala
renormalizar (h:caso) =
  let (posterior,antes) = splitAt (long-h) (h:caso)
      (haches,despues) = span (==h) posterior
  in (map (\i -> if i > h then i-long else i) antes)
  ++ haches
  ++ (map (\i -> if i <= h then i+long else i) despues)

unCaso :: Escala -> [(Int,Int)] -> Escala -> Escala
unCaso _ _ [] = []
unCaso _ [] r = r
unCaso [] ((next, nextfrec):frecs) start =
  unCaso [] frecs $
    start ++ replicate nextfrec next
unCaso (f:fixed) ((next, nextfrec):frecs) (s:start) =
  if f == mod (1 + s + length start) long -- f es el siguiente índice
    then if next == f --si no es igual no está fijado
      then unCaso fixed ((if nextfrec == 1 then [] else [(next, nextfrec-1)]) ++ frecs) $
           (s:start) ++ [next]
      else []
    else if next <= f --no me he quedado corta
      then unCaso (f:fixed) ((if nextfrec == 1 then [] else [(next, nextfrec-1)]) ++ frecs) $
           (s:start) ++ [next]
      else []
