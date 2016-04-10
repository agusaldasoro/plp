-- Para correr los tests:
-- ghc Tests-alu.hs && ./Tests-alu

import Solucion
import Test.HUnit
import Data.List

mucho = 1000000

-- evaluar main para correr todos los tests
main = runTestTT allTests

allTests = test [
 	"split" ~: testsSplit,
  "longPromedioPalabras" ~: testsLongitudPromedioPalabras,
  "frecuenciaTokens" ~: testsFrecuenciaTokens,
  "distEuclediana" ~: testsDistEuclediana,
  "distCoseno" ~: testsDistCoseno,
  "acurracy" ~: testsAcurracy,
  "cuentas" ~: testsCuentas,
  "normalizarExtractor" ~: testsNormalizarExtractor,
  "knn" ~: testsknn,
  "repeticionesPromedio" ~: testsRepeticionesPromedio,
  "extraerFeatures" ~: testsExtraerFeatures,
  "extraerSepararDatos" ~: testsSepararDatos
  ]

testsLongitudPromedioPalabras = test [
  longitudPromedioPalabras "Este test tiene palabras $$++$$" ~?= 5.4,
  longitudPromedioPalabras "1" ~?= 1,
  longitudPromedioPalabras "Hola a todo" ~?= 3,
  longitudPromedioPalabras "3 tristes tigres" ~?= 4.6666665,
  longitudPromedioPalabras "Mi mama me mima" ~?= 3,
  longitudPromedioPalabras [if x `mod` 2 == 0 then ' ' else 'a' | x <- [1 .. mucho]] ~?= 1
  ]

testsFrecuenciaTokens = test [
  buscarExtractor '_' "use_snake_case !" ~?= 0.125,
  buscarExtractor '!' "use_snake_case !" ~?= 0.0625,
  buscarExtractor 's' "use_snake_case !" ~?= 0.1875,
  buscarExtractor 'h' "use_snake_case !" ~?= 0.0,
  buscarExtractor 'a' (replicate mucho 'a') ~?= 1
  ]

testsDistEuclediana = test [
  distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5] ~?= 0.47186464,
  distEuclideana [1.0] [1.0] ~?= 0,
  distEuclideana [14.0, 9.0] [9.0, 14.0] ~?= 7.071068,
  distEuclideana (replicate mucho 1.0) (replicate mucho 0.0) ~?= sqrt (fromIntegral mucho)
  ]

testsDistCoseno = test [
  distCoseno [1.0, 2.0, 3.0] [1.0, 2.0, 3.0] ~?= 0.99999994,
  distCoseno [1.0] [2.0] ~?= 1,
  distCoseno (replicate mucho 1.0) (replicate mucho 2.0) ~?= 1.0
  ]

testsAcurracy = test [
  accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"] ~?= 0.6,
  accuracy ["f", "i"] ["f", "f"] ~?= 0.5,
  accuracy ["f", "f", "i", "i", "f"] ["f", "f", "i", "i", "f"] ~?= 1.0,
  accuracy (replicate mucho "f") [if x `mod` 2 == 0 then "f" else "i" | x <- [1 .. mucho]] ~?= 0.5
  ]

buscarExtractor = \tok ->
        let posToken = (\(Just i) -> i) (elemIndex tok tokens)
        in frecuenciaTokens !! posToken

testsCuentas = test [
	cuentas ["x","x","y","x","z"] ~?= [(3,"x"), (1,"y"), (1,"z")],
	cuentas [""] ~?= [(1,"")],
	cuentas ["e","3", "6", "3"] ~?= [(1,"e"), (2, "3"), (1,"6")],
	cuentas ([] :: [Int]) ~?= [],
	cuentas [if 1 == mod a 2 then "a" else "b"| a <- [1..100000]] ~?= [(50000, "a"), (50000, "b")]

	]

testsNormalizarExtractor = test [
 	normalizarExtractor ["usando haskell la vida es hermosa", "shalalala", "el que lee este test no entiende nada", "a a a a"] repeticionesPromedio "a a a a" ~?= 1.0,
 	normalizarExtractor ["usando haskell la vida es hermosa","e e", "shalalala", "el que lee este test no entiende nada", "a a a a"] repeticionesPromedio "shalalala" ~?= 0.25,
 	normalizarExtractor ["usando haskell la vida es hermosa","e e", "shalalala", "el que lee este test no entiende nada", "a a a a"] repeticionesPromedio "e e" ~?= 0.5
 	]

testsknn = test [
	(knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "f",
	(knn 4 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1] ~?= "i",
	(knn 120 [[a, a*32 ]| a <- [1..10000]] [if mod a 3 == 0 then "i" else "f" | a <- [1..10000]] distEuclideana) [1,63] ~?= "f",
	(knn 5000 [[a, a*32 ]| a <- [1..10000]] [if mod a 3 == 0 then "i" else "f" | a <- [1..10000]] distEuclideana) [1,63] ~?= "f",
	(knn 50000 [[a, a*32 ]| a <- [1..100000]] [if mod a 6 == 0 then "i" else "f" | a <- [1..10000]] distEuclideana) [9000,63] ~?= "f"
	]

testsSplit = test [
  split ',' ",PLP," ~?= ["PLP"],
  split ',' " ,PLP, " ~?= [" ","PLP"," "],
  split ',' "hola PLP, bienvenidos!" ~?= ["hola PLP"," bienvenidos!"],
  split ' ' "" ~?= [],
  split 84 [] ~?= [],
  split ['a'] [[]] ~?= [[""]],
  split 'a' "Estaba la PAloma BlanCA" ~?= ["Est","b"," l"," PAlom"," Bl","nCA"],
  split 'a' [if x `mod` 2 == 0 then ' ' else 'a' | x <- [1 .. 100000]] ~?= [" " | x <- [1 .. 50000]]
  ]

testsRepeticionesPromedio = test [
  repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$" ~?= 2.5,
  repeticionesPromedio "a b c d" ~?= 1,
  repeticionesPromedio "aba aba aba aca" ~?= 2,
  repeticionesPromedio [if x `mod` 2 == 0 then ' ' else 'a' | x <- [1 .. 100000]] ~?= 50000
  ]

testsExtraerFeatures = test [
  extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"] ~?= [[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]],
  extraerFeatures [] ["a"] ~?= [[]],
  extraerFeatures [longitudPromedioPalabras] [] ~?= [],
  extraerFeatures [repeticionesPromedio, repeticionesPromedio] [[if x `mod` 2 == 0 then ' ' else 'a' | x <- [1 .. 100000]], [if x `mod` 2 == 0 then ' ' else 'a' | x <- [1 .. 100000]]] ~?= [[1.0,1.0],[1.0,1.0]]
  ]

testsSepararDatos = test [
  separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] ["1","2","3","4","5","6","7"] 3 2 ~?= ([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]],[[3.0,3.0],[4.0,4.0]],["1","2","5","6"],["3","4"]),
  separarDatos [[x,x] | x <- [1 .. 100003]] ["x" | x <- [1 .. 100003]] 4 2 ~?= ([[x,x] | x <- [1 .. 25000]] ++ [[x,x] | x <- [50001 .. 100000]], [[x,x] | x <- [25001 .. 50000]], ["x" | x <- [1 .. 25000]] ++ ["x" | x <- [50001 .. 100000]], ["x" | x <- [25001 .. 50000]])
  ]
