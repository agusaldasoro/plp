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

-----------------
-- Ejercicio 1 --
-----------------

--delim es el elemento separador, xs la lista a aplicar el split.
split :: Eq a => a -> [a] -> [[a]]
split = \delim xs -> sinVacios $ foldr (f delim) [[]] xs
--Primero se aplica f con delim que lo que hace es partir en listas separando por el delimitador.
--Y luego se retiran las listas en blanco con sinVacios
        where sinVacios = filter (not . null)
              f delim actual (x:xs) =
                if delim == actual then [] : (x : xs) else (actual : x) : xs

-----------------
-- Ejercicio 2 --
-----------------

--A cada palabra se le pide su tamanio, y luego se hace un promedio de ellos.
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = \texto -> mean $ map genericLength $ palabras texto

--Se divide spliteando las palabras por el caracter espacio.
palabras :: Texto -> [Texto]
palabras = split ' '

-----------------
-- Ejercicio 3 --
-----------------

--Devuelve la cantidad de apariciones por cada elemento de la lista.
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = \xs ->
--Primero se obtiene la lista sin repetidos de xs,
--y luego por cada uno de estos elementos se pregunta cuantas veces aparece en la lista xs original.
        let repeticiones = map (contarRepeticiones xs) sinRepetidos
            sinRepetidos = sacarRepetidos xs
        in zip repeticiones sinRepetidos

--Saca los elementos repetidos de una lista.
sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos xs = foldl f [] xs
--Si el elemento no aparece en la lista, lo agrega. Sino, no.
        where f rec x = if x `elem` rec then rec else rec ++ [x]

--Cuenta la cantidad de veces que aparece el elemento en la lista
contarRepeticiones :: Eq a => [a] -> a -> Int
contarRepeticiones lista elemento = foldr (+) 0 $ map (beta . (==elemento)) lista
--A cada elemento de la lista se le pregunta si es igual, y luego se "suman los true".
        where beta = \x -> if x then 1 else 0

-----------------
-- Ejercicio 4 --
-----------------

--Se calcula el promedio de apariciones en la lista de cada palabra (separada por espacios).
repeticionesPromedio :: Extractor
repeticionesPromedio = \xs -> mean $ map (fromIntegral . fst) $ cuentas $ palabras xs

-----------------
-- Ejercicio 5 --
-----------------

--Devuelve cual es la frecuencia relativa de cada token de la lista.
frecuenciaTokens :: [Extractor]
frecuenciaTokens = map frecuenciaRelativa tokens

--Dados un token y un texto, devuelve las repeticiones de dicho token dividido el tamanio del texto.
frecuenciaRelativa :: Char -> Extractor
frecuenciaRelativa = \token texto ->
--total es el tamanio del texto.
--repeticionesDelToken es la cantidad de veces que aparece dicho token en el texto.
--aparicionesDelToken salva el caso donde el token no aparece y repeticionesDelToken es la lista vacia.
        let total = (fromIntegral . length) texto
            repeticionesDelToken = filter (\tupla -> (snd tupla) == token) $ cuentas texto
            extraerToken = if not . null $ repeticionesDelToken then head repeticionesDelToken else (0 , token)
            aparicionesDelToken = fromIntegral $ fst $ extraerToken
        in  aparicionesDelToken / total

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-----------------
-- Ejercicio 6 --
-----------------

--Toma por cada valor obtenido del extractor su valor absoluto,
--para luego dividirlo por el maximo del texto y asi tener el vector normalizado.
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = \textos ext texto -> (ext texto) / (maximum $ map (abs . ext) textos)

-----------------
-- Ejercicio 7 --
-----------------

--Primero se normalizan los extractores pasados como parametro.
--Luego, se calculan para todo el texto.
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = \es textos -> map (\texto -> map ($ texto) (map (normalizarExtractor textos) es)) textos

-----------------
-- Ejercicio 8 --
-----------------

--Es la raiz de la sumatoria de los cuadrados de la resta coordenada a coordenada.
distEuclideana :: Medida
distEuclideana = \xs ys -> sqrt $ sum $ map (\tupla -> ((fst tupla) - (snd tupla))^2) $ zip xs ys

--Producto Escalar entre los dos vectores, dividido por la multiplicacion de sus normas.
distCoseno :: Medida
distCoseno = \xs ys -> (productoEscalar xs ys) / ((normaVectorial xs) * (normaVectorial ys))

productoEscalar :: Medida
productoEscalar xs ys = sum $ map (\tupla -> (fst tupla)*(snd tupla)) $ zip xs ys

normaVectorial :: Instancia -> Float
normaVectorial xs = sqrt $ productoEscalar xs xs

-----------------
-- Ejercicio 9 --
-----------------

--Toma a la etiqueta con mayor cantidad de apariciones en los k mas cercanos a la instanciaAEtiquetar.
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = \k datos etiquetas medida instanciaAEtiquetar ->
--Calcula todas las distancias desde la instanciaAEtiquetar a etiquetar con los datos.
--Luego, toma al de mayor cantidad de apariciones entre los k mas cercanos.
        let distancias = calcularDistancias medida datos instanciaAEtiquetar
        in (masApariciones . (take k) . sort) $ zip distancias etiquetas

--Dada una instancia, se calculan todas las distancias hacia los demas datos.
calcularDistancias :: Medida -> Datos -> Instancia -> Instancia
calcularDistancias = \medida datos instancia -> map (medida instancia) datos

--Devuelve la etiqueta que tenga mas apariciones en la lista.
masApariciones :: [(Feature, Etiqueta)] -> Etiqueta
masApariciones = \xs -> snd $ maximum $ cuentas $ map snd xs

------------------
-- Ejercicio 10 --
------------------

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

------------------
-- Ejercicio 11 --
------------------

accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = \xs ys -> mean $ map (beta . \tupla -> snd tupla == fst tupla) $ zip xs ys
        where beta = \x -> if x then 1 else 0

------------------
-- Ejercicio 12 --
------------------

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

-- aplicarModelo = \tupla -> (etiVal tupla, map (knn 15 (datosEnt tupla) (etiEnt tupla) distEuclideana) (datosVal tupla))

datosEnt (x, _, _, _) = x
datosVal (_, x, _, _) = x
etiEnt (_, _, x, _) = x
etiVal (_, _, _, x) = x
