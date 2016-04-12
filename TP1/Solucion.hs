module Solucion where

import Data.List
import Data.Ord

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

--delim es el elemento separador
split :: Eq a => a -> [a] -> [[a]]
split = \delim ->
--Si el elemento actual es igual al delimitador, quiero crear una nueva lista: Creo la lista vacia.
--Sino, sigo agregando a la lista anterior.
        let f = \actual (x:xs) -> if delim == actual then [] : (x : xs) else (actual : x) : xs
        in filter (not . null) . foldr f [[]]

-----------------
-- Ejercicio 2 --
-----------------

--A cada palabra se le pide su tamanio, y luego se hace un promedio de ellos.
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras = mean . map genericLength . palabras

--Se dividen las palabras por el caracter espacio.
palabras :: Texto -> [Texto]
palabras = split ' '

-----------------
-- Ejercicio 3 --
-----------------

--Devuelve la cantidad de apariciones por cada elemento de la lista (sin repetidos).
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas = \xs -> map (\x -> (contarRepeticiones x xs, x)) $ nub xs

--Cuenta la cantidad de veces que aparece el elemento en la lista.
contarRepeticiones :: Eq a => a -> [a] -> Int
contarRepeticiones = \elemento -> length . filter (==elemento)

-----------------
-- Ejercicio 4 --
-----------------

--Se calcula el promedio de apariciones en la lista de cada palabra.
repeticionesPromedio :: Extractor
repeticionesPromedio = mean . map (fromIntegral . fst) . cuentas . palabras

-----------------
-- Ejercicio 5 --
-----------------

--Devuelve cual es la frecuencia relativa de cada token de la lista.
frecuenciaTokens :: [Extractor]
frecuenciaTokens =
--frecuenciaRelativa, dados un token y un texto, calcula la cantidad de apariciones del token dividido el tamanio del texto.
          let frecuenciaRelativa = \token texto -> (fromIntegral $ contarRepeticiones token texto) / (fromIntegral . length) texto
          in map frecuenciaRelativa tokens

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

-----------------
-- Ejercicio 6 --
-----------------

--Compone al extractor con la division del valor absoluto maximo y su resultado.
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor = \textos ext ->
--extraerAbs es la funcion del extractor devolviendo el valor absoluto.
--valorAbsMax es la funcion que dado un texto, encuentra el caracter con mayor valor absoluto en el extractor.
        let extraerAbs =  (abs . ext)
            valorAbsMax = extraerAbs $ maximumBy (comparing extraerAbs) textos
        in (/valorAbsMax) . ext

-----------------
-- Ejercicio 7 --
-----------------

--Primero se normalizan los extractores pasados como parametro.
--Luego, se calculan para todo el texto.
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures = \es textos ->
        let extractoresNormalizados = map (normalizarExtractor textos) es
        in map (\texto -> map (\extractor -> extractor texto) extractoresNormalizados) textos

-----------------
-- Ejercicio 8 --
-----------------

--Es la raiz de la sumatoria de los cuadrados de la resta coordenada a coordenada.
distEuclideana :: Medida
distEuclideana = \xs -> sqrt . sum . zipWith (\x y -> (x - y)^2) xs

--Producto Escalar entre los dos vectores, dividido por la multiplicacion de sus normas.
distCoseno :: Medida
distCoseno = \xs ys -> (productoEscalar xs ys) / ((normaVectorial xs) * (normaVectorial ys))

--Es el producto coordenada a coordenada.
productoEscalar :: Medida
productoEscalar = \xs -> sum . zipWith (\x y -> x * y) xs

--La raiz del producto escalar.
normaVectorial :: Instancia -> Float
normaVectorial = \xs -> sqrt $ productoEscalar xs xs

-----------------
-- Ejercicio 9 --
-----------------

--Toma a la etiqueta con mayor cantidad de apariciones en los k mas cercanos a la instanciaAEtiquetar.
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn = \k datos etiquetas medida ->
--masApariciones elige de una [(Int, Etiqueta)] que marca la cantidad de apariciones, la etiqueta con mayor valor.
        let masApariciones = snd . maximum . cuentas . map fst
--Dada una instancia, calcula la distancia a los datos, luego etiqueta cada distancia, para elegir a las k mas cercanas.
        in masApariciones . (take k) . sortBy(comparing snd) . (zip etiquetas) . (\instancia -> map (medida instancia) datos)

------------------
-- Ejercicio 10 --
------------------

--Se asume que los datos y etiquetas pasados por parametro tienen el mismo tamanio.
--Se separa cada uno de ellos en dos particiones, para armar los datos de entrenamiento y los de prueba
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos = \xs y n p ->
      let t = (length xs) `div` n
          finPrimera    = (p - 1) * t
          finValidacion = p * t
          finSegunda    = n * t
      in        (sublista 1 finPrimera xs ++ sublista (finValidacion + 1) finSegunda xs,
                 sublista (finPrimera + 1) finValidacion xs,
                 sublista 1 finPrimera y ++ sublista (finValidacion + 1) finSegunda y,
                 sublista (finPrimera + 1) finValidacion y)

--Dados dos indices i j y una lista, devuelve la sublista[i..j]
sublista :: Int -> Int -> [a] -> [a]
sublista = \comienzo fin -> take (fin - comienzo + 1) . drop (comienzo - 1)


------------------
-- Ejercicio 11 --
------------------

--Dadas dos lista, se comparan. Devuelve el promedio de error (diferencia).
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy = \xs ->
        let beta = \x -> if x then 1 else 0
        in mean . zipWith (\x -> beta . (==x)) xs

------------------
-- Ejercicio 12 --
------------------

--Calcula el promedio del accuracy de tomar todas las particiones de tamanio n como valores a predecir, dejando los demas como entrenamiento.
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation = \n datos etiquetas ->
--analizarTupla toma una tupla (datosEnt,datosVal,etiEnt,etiVal) y calcula la prediccion con knn (y los parametros indicados en el enunciado),
--calculando luego su accuracy
        let analizarTupla = \tupla -> accuracy (map (knn 15 (datosEnt tupla) (etiEnt tupla) distEuclideana) (datosVal tupla)) (etiVal tupla)
--Separa los datos en n utilizando cada una de las particiones para analizar esa tupla y calcular su promedio.
        in mean $ map (analizarTupla . (separarDatos datos etiquetas n)) [1 .. n]

datosEnt (x, _, _, _) = x
datosVal (_, x, _, _) = x
etiEnt   (_, _, x, _) = x
etiVal   (_, _, _, x) = x
