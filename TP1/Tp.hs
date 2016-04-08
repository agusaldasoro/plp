module Tp where

import Data.List

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

-- Ejercicio 1 --

split :: Eq a => a -> [a] -> [[a]]
split = (\delim xs -> filter (not . null) (foldr (separarSegunDelim delim) [[]] xs))

separarSegunDelim :: Eq a => a -> a -> [[a]] -> [[a]]
separarSegunDelim delim actual rec =
  if delim == actual then
    []:rec
  else
    ((actual : (head rec)) : tail rec)

-- Ejercicio 2 --

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = (\texto -> mean (map genericLength (split ' ' texto)))

-- Ejercicio 3 --

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = (\xs -> sacarRepetidos (zip (map (contar xs) xs) xs))

sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos xs = foldl f [] xs
  where f rec x = if x `elem` rec then rec else rec ++ [x]

contar :: Eq a => [a] -> a -> Int
contar lista elemento = foldr (+) 0 (map beta esIgual)
          where esIgual = map (==elemento) lista

beta :: Bool -> Int
beta True = 1
beta False = 0

-- Ejercicio 4 --

repeticionesPromedio :: Extractor
repeticionesPromedio = (\xs -> mean (map (fromIntegral. fst) (cuentas (split ' ' xs))))

-- Ejercicio 5 --

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frecuenciaRelativa tokens

frecuenciaRelativa :: Char -> Extractor
frecuenciaRelativa token texto = fromIntegral(fst(head(filter (segundoEsIgual token) (cuentas texto)))) / (fromIntegral (length texto))
  where segundoEsIgual token = (\tupla -> (snd tupla) == token)

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- Ejercicio 6 --

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos ext = (\texto -> (ext texto) / maximo)
  where maximo = maximum (map (abs . ext) textos)

-- Ejercicio 7 --

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = \es ts -> map (extraer (extractoresNormalizados es ts)) ts 
    where
      extractoresNormalizados es ts = map (normalizarExtractor ts) es
      extraer es t = map ($ t) es

-- Ejercicio 8 --

distEuclideana :: Medida
distEuclideana = undefined

distCoseno :: Medida
distCoseno = undefined

-- Ejercicio 9 --

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas medida = (\instanciaAEtiquetar -> (masApariciones. (take k) .sort) (zip (distancias instanciaAEtiquetar) etiquetas))
                                                            where distancias = calcularDistancias medida datos

calcularDistancias :: Medida -> Datos -> Instancia -> Instancia
calcularDistancias medida datos instancia = map (medida instancia) datos

masApariciones :: [(Feature, Etiqueta)] -> Etiqueta
masApariciones xs = (snd.head.sort) (map (contarApariciones xs) xs)

contarApariciones ::  [(Feature, Etiqueta)] -> (Feature, Etiqueta) -> (Int, Etiqueta)
contarApariciones xs x = (contar (map snd xs) (snd x), snd x)

-- Ejercicio 10 --

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos xs y n p = (entrenamiento q xs m r, validacion q m xs, entrenamiento q y m r, validacion q m y)
      where t = (length xs) `div` n
            m = p * t
            q = (p - 1) * t
            r = n * t

entrenamiento :: Int -> [a] -> Int -> Int -> [a]
entrenamiento q xs m r = (take q xs) ++ (drop m (take r xs))

validacion :: Int -> Int -> [a] -> [a]
validacion q m xs = drop q (take m xs)

-- Ejercicio 11 --

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

-- Ejercicio 12 --

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
