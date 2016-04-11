module Solucion where

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
cuentas = \xs ->
        let repeticiones = map (contarRepeticiones xs) sinRepetidos
            sinRepetidos = sacarRepetidos xs
        in zip repeticiones sinRepetidos

sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos xs = foldl f [] xs
        where f rec x = if x `elem` rec then rec else rec ++ [x]

contarRepeticiones :: Eq a => [a] -> a -> Int
contarRepeticiones lista elemento = foldr (+) 0 $ map (beta . (==elemento)) lista
        where beta = \x -> if x then 1 else 0

-- Ejercicio 4 --

repeticionesPromedio :: Extractor
repeticionesPromedio = \xs -> mean $ map (fromIntegral . fst) $ cuentas $ palabras xs

-- Ejercicio 5 --

frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frecuenciaRelativa tokens

frecuenciaRelativa :: Char -> Extractor
frecuenciaRelativa = \token texto ->
        let total = (fromIntegral . length) texto
            repeticionesDelToken = filter (\tupla -> (snd tupla) == token) $ cuentas texto
            extraerToken = if not . null $ repeticionesDelToken then head repeticionesDelToken else (0 , token)
            aparicionesDelToken = fromIntegral $ fst $ extraerToken
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
distEuclideana = \xs ys -> sqrt $ sum $ map (\tupla -> ((fst tupla) - (snd tupla))^2) $ zip xs ys

distCoseno :: Medida
distCoseno = \xs ys -> (productoEscalar xs ys) / ((normaVectorial xs) * (normaVectorial ys))

productoEscalar :: Medida
productoEscalar xs ys = sum $ map (\tupla -> (fst tupla)*(snd tupla)) $ zip xs ys

normaVectorial :: Instancia -> Float
normaVectorial xs = sqrt $ productoEscalar xs xs

-- Ejercicio 9 --

knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = \k datos etiquetas medida instanciaAEtiquetar ->
        let distancias = calcularDistancias medida datos instanciaAEtiquetar
        in (masApariciones . (take k) . sort) $ zip distancias etiquetas

calcularDistancias :: Medida -> Datos -> Instancia -> Instancia
calcularDistancias = \medida datos instancia -> map (medida instancia) datos

masApariciones :: [(Feature, Etiqueta)] -> Etiqueta
masApariciones = \xs -> (snd . last  . sort) $ map (contarApariciones xs) xs

contarApariciones ::  [(Feature, Etiqueta)] -> (Feature, Etiqueta) -> (Int, Etiqueta)
contarApariciones = \xs x -> (contarRepeticiones (map snd xs) (snd x), snd x)

-- Ejercicio 10 --

separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = \xs y n p ->
              let todoSeparado = separar xs y n
              in tuplar todoSeparado p

deleteAt :: Int -> [a] -> [a]
deleteAt p xs = take (p - 1) xs ++ drop p xs

tuplar :: ([Datos], [[Etiqueta]]) -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
tuplar todoSeparado p = let datosValidacion = fst todoSeparado !! (p - 1)
                            datosEntrenamiento = concat $ deleteAt p $ fst todoSeparado
                            etiquetasValidacion = snd todoSeparado !! (p - 1)
                            etiquetasEntrenamiento = concat $ deleteAt p $ snd todoSeparado
                        in (datosEntrenamiento, datosValidacion, etiquetasEntrenamiento, etiquetasValidacion)

separar :: Datos -> [Etiqueta] -> Int -> ([Datos], [[Etiqueta]])
separar ds es n = let t = (length ds) `div` n
                      filtrarSiSobra = \xs -> if length ds `mod` n /= 0 then init xs else xs
                  in (filtrarSiSobra $ splitEvery t ds, filtrarSiSobra $ splitEvery t es)

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- Ejercicio 11 --

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = \xs ys -> mean $ map (beta . \tupla -> snd tupla == fst tupla) $ zip xs ys
        where beta = \x -> if x then 1 else 0

-- Ejercicio 12 --

nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = \n datos etiquetas ->
        let ps = [1 .. n]
            datosSeparados = separar datos etiquetas n
            -- generarTupla p = separarDatos datos etiquetas n p
            generarTupla p = tuplar datosSeparados p
            calcularAccuracy = \tupla -> accuracy (fst tupla) (snd tupla)
            aplicarModelo = \tupla ->
              let datosEntrenamiento = datosEnt tupla
                  etiquetaEntrenamiento = etiEnt tupla
                  datosValidacion = datosVal tupla
                  etiquetaValidacion = etiVal tupla
                  modelo = knn 15 datosEntrenamiento etiquetaEntrenamiento distEuclideana
              in (etiquetaValidacion, map modelo datosValidacion)
        in mean $ map (calcularAccuracy . aplicarModelo . generarTupla) ps

datosEnt (x, _, _, _) = x
datosVal (_, x, _, _) = x
etiEnt (_, _, x, _) = x
etiVal (_, _, _, x) = x


-- aplicarModelo = \tupla -> (etiVal tupla, map (knn 15 (datosEnt tupla) (etiEnt tupla) distEuclideana) (datosVal tupla))
