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
split = \delim xs -> sinVacios $ foldr (f delim) [[]] xs
        where sinVacios = filter (not . null)
              f delim actual (x:xs) =
                if delim == actual then [] : (x : xs) else (actual : x) : xs


-- Ejercicio 2 --

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = \texto -> mean $ map genericLength $ palabras texto

palabras :: Texto -> [Texto]
palabras = split ' '

-- Ejercicio 3 --

cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = \xs -> sacarRepetidos $ zip (repeticiones xs) xs
        where repeticiones = \xs -> map (contarRepeticiones xs) xs

sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos xs = foldl f [] xs
        where f rec x = if x `elem` rec then rec else rec ++ [x]

contarRepeticiones :: Eq a => [a] -> a -> Int
contarRepeticiones lista elemento = foldr (+) 0 $ map (beta . (==elemento)) lista
        where beta = \x -> if x then 1 else 0

-- Ejercicio 4 --

repeticionesPromedio :: Extractor
repeticionesPromedio = \xs -> mean $ map (fromIntegral. fst) $ cuentas $ palabras xs

-- Ejercicio 5 --

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frecuenciaRelativa tokens

frecuenciaRelativa :: Char -> Extractor
frecuenciaRelativa = \token texto ->
        let total = (fromIntegral . length) texto
            repeticionesDelToken = head $ filter (\tupla -> (snd tupla) == token) $ cuentas texto
            aparicionesDelToken = fromIntegral $ fst $ repeticionesDelToken 
        in  aparicionesDelToken / total

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-- Ejercicio 6 --

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = \textos ext texto ->
        let maximo = maximum $ map (abs . ext) textos
        in (ext texto) / maximo

-- Ejercicio 7 --

extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = \es textos ->
        let extractoresNormalizados = map (normalizarExtractor textos) es
            extraer = \texto -> map ($ texto) extractoresNormalizados
        in map extraer textos

-- Ejercicio 8 --

distEuclideana :: Medida
distEuclideana = undefined

distCoseno :: Medida
distCoseno = undefined

-- Ejercicio 9 --

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = \k datos etiquetas medida instanciaAEtiquetar ->
        let distancias = calcularDistancias medida datos instanciaAEtiquetar
        in (masApariciones . (take k) . sort) $ zip distancias etiquetas


calcularDistancias :: Medida -> Datos -> Instancia -> Instancia
calcularDistancias medida datos instancia = map (medida instancia) datos

masApariciones :: [(Feature, Etiqueta)] -> Etiqueta
masApariciones xs = (snd . head . sort) $ map (contarApariciones xs) xs

contarApariciones ::  [(Feature, Etiqueta)] -> (Feature, Etiqueta) -> (Int, Etiqueta)
contarApariciones xs x = (contarRepeticiones (map snd xs) (snd x), snd x)

-- Ejercicio 10 --

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos xs y n p = (entrenamiento q xs m r, validacion q m xs, entrenamiento q y m r, validacion q m y)
      where t = (length xs) `div` n
            m = p * t
            q = (p - 1) * t
            r = n * t

entrenamiento :: Int -> [a] -> Int -> Int -> [a]
entrenamiento q xs m r = (take q xs) ++ (drop m $ take r xs)

validacion :: Int -> Int -> [a] -> [a]
validacion q m xs = drop q $ take m xs

-- Ejercicio 11 --

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = undefined

-- Ejercicio 12 --

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = undefined
